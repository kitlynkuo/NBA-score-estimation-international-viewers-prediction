# -*- coding: utf-8 -*-
"""
Created on Wed Jul 11 17:26:07 2018

@author: 王仁信
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Jul  5 22:37:51 2018

@author: 王仁信
"""
    
import Player
import csv
    
with open('plusminus_OT.csv', 'w', newline='') as csvfile:
    outfile = csv.writer(csvfile, delimiter=',')
      
    with open("Play by Play_OT.csv") as f1:
        PLAY = list(csv.reader(f1))    # list[475] with each element is a list   
            
        with open("Lineup_all_OT.csv") as f2:
            LINEUP = list(csv.reader(f2))  
    
            # initial
            game_id = ''
            gamenum = 0
            player_id = []      # record player ever played
            player_obj = []     # create player object
            sub_team_id = ''
            foul = False
            sub = False
            lastft = ['10', '12', '15', '19', '20', '22', '26', '29'] #, '16'
            
            # time line, start from Event_Num 0
            for event in PLAY:
                # new game
                if game_id != event[0]:
                    for player in player_obj:        
                        outfile.writerow([game_id] + [player.pid] + [player.ptdiff])  
                        
                    game_id = event[0]
                    gamenum += 1
                    player_id = []      # record player ever played
                    player_obj = []     # create player object
                    sub_team_id = ''
                    foul = False
                    sub = False  
                   
                
                
                period = int(event[1])
                if foul and sub and (event[3] == '1' or event[3] == '2'):
                    for player in player_obj:
    #                    if player.status == 1:
                        player.endfoul()                
                    foul = False
                    sub = False
                    
                    
                if event[3] == '12':          # start of period, add player if never exist
#                    print("Start of period", period)
                    for row in range((gamenum-1)*50 + (period-1)*10, (gamenum-1)*50 + period*10):   
                        if LINEUP[row][2] not in player_id:
                            # create obj for each ounew player
                            temp = Player.Player(LINEUP[row][3], LINEUP[row][2])
                            player_obj.append(temp)
                            player_id.append(LINEUP[row][2])
                        elif LINEUP[row][2] in player_id:
                            for num in range(len(player_obj)):
                                if player_obj[num].pid == LINEUP[row][2]:
                                    player_obj[num].subin()                                                          
                elif event[3] == '13':        # end of period, Player.subout
#                    print("End of period", period)
                    for num in range(len(player_obj)):
                        player_obj[num].subout()  
                        player_obj[num].onfoul = 0
                        
                elif event[3] == '1':                           # made shot            
    #                print("made shot")
                    for player in player_obj:
                        if player.pid == event[9]:
                            player.add_total_pts(int(event[6]))
                        if player.status == 1:
                            player.change_pt_diff(event[7], int(event[6]))                        
    
                elif event[3] == '3' and event[6] == '1':       # finish of free throw
                    for player in player_obj:
                        if player.pid == event[9]:
                            player.add_total_pts(int(event[6]))
                        
                        if event[4] in lastft:
                            if player.onfoul == 1:
                                player.change_pt_diff(event[7], int(event[6]))
                                player.endfoul() 
                                
                            foul = False  
                            sub = False
                        else:
                            if player.onfoul == 1:
                                player.change_pt_diff(event[7], int(event[6]))
                
                elif event[3] == '6':                           # foul
                    foul = True
                    for player in player_obj:
                        if player.status == 1:
                            player.foul()
                                                    
                elif event[3] == '8':                           # substitution
                    sub = True
                    # subout person1
                    for player in player_obj:
                        if player.pid == event[9]:
                            player.subout()
                            sub_team_id = player.team
                    # subin person2
                    if event[10] not in player_id:                
                        temp = Player.Player(sub_team_id, event[10])
                        player_obj.append(temp)
                        player_id.append(event[10])                    
                    else:
                        for player in player_obj: 
                            if player.pid == event[10]:
                                player.subin()                   
     
        # output final score and player stats
        final = dict()
        for player in player_obj:
            print(player)
            final[player.team] = final.get(player.team, 0) + player.total_score
        print(final)
            
            
     
            
    
        

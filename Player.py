# -*- coding: utf-8 -*-
"""
Created on Mon Jul  2 08:32:15 2018

@author: kpang
"""

class Player:

    def __init__(self, team='', pid=''):
        self.team = team
        self.pid = pid
        self.status = 1
        self.onfoul = 0
        self.ptdiff = 0
        self.total_score = 0
        
    def subout(self):
        self.status = 0
        
    def subin(self):
        self.status = 1
        self.onfoul = 0
    
    def foul(self):
        self.onfoul = 1
        
    def endfoul(self):
        self.onfoul = 0

    def change_pt_diff(self, team, amt):
        if team != self.team:
            self.ptdiff -= amt
        else:
            self.ptdiff += amt

    def add_total_pts(self, amt):
        self.total_score += amt
        
    
    def __str__(self):
        return 'TEAM: {0}\n PLAYER_ID: {1}\n pts: {2}, pts diff: {3}, status: {4}, onfoul: {5}\n'.format(self.team[0:5], self.pid[0:5], self.total_score,self.ptdiff, self.status, self.onfoul)

               
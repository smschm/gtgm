from twisted.web import xmlrpc, server
import sys
import random

class GTPC(xmlrpc.XMLRPC):
    def xmlrpc_ping(self):
        return True

    def xmlrpc_startGame(self):
        return True # assume we want to start

    def xmlrpc_initialize(self, game_id, opponent_id, player_num, hand):
        # collecting data into our struct (which we won't actually use):
        self.game_id = game_id
        self.opponent_id = opponent_id
        self.player_num = player_num
        self.hand = hand
        return True

    def xmlrpc_getPlay(self, hand, discards, expos, decksize):
       
        our_expos = expos[0]
        their_expos = expos[1]
        # if we are player 1, the above is not correct;
        # we have to swap them
        if self.player_num == 1:
            our_expos, their_expos = their_expos, our_expos

        # pick a random card from our hand (which should always have 8):
        ix = random.randint(0,7)

        # always try to play to an expedition (represented by 1)
        # (if the play is illegal, the card is automatically discarded
        # by the GM instead)
        pt = 1
       
        # pick from a random pile selected from the deck (-1) or
        # a discard pile (0,1,2,3,4)
        # note if an empty discard pile is chosen, the GM automatically
        # draws from the deck for you.
        df = random.randint(-1,4)
        return {'card_ix': ix , 'play_to': pt , 'draw_from': df }

    def xmlrpc_opponentPlay(self, card_played, play_to, draw_from):
        # the random bot doesn't care
        return True

    def xmlrpc_gameEnd(self, score0, score1):
        if self.player_num == 1:
            score1, score0 = score0, score1
        print "score: us: %d them: %d" % (score0, score1)
        return True

if __name__ == '__main__':
    print sys.argv
    from twisted.internet import reactor
    r = GTPC()
    reactor.listenTCP(int(sys.argv[1]), server.Site(r))
    reactor.run()

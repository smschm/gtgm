from twisted.web import xmlrpc, server
import sys
import random

nsuits = 5

def format_card(card):
    #print card
    #try:
    card_symbols = ['<i>','ERR',' 2 ',' 3 ',' 4 ',' 5 ',' 6 ',' 7 ',' 8 ',' 9 ',' 10']
    suit_codes = ['1;43','1;44','7','1;42','1;41']
    sys.stdout.write("\033[%sm%s\033[0m " % (suit_codes[card['suit']], card_symbols[card['rank']]))
    #except Exception:
    #    sys.stdout.write('*')

class GTPC(xmlrpc.XMLRPC):
    def xmlrpc_startGame(self):
        return True # assume we want to start

    def xmlrpc_initialize(self, game_id, opponent_id, player_num, hand):
        self.game_id = game_id
        self.opponent_id = opponent_id
        self.player_num = player_num
        self.hand = hand
        print "we are player #", player_num
        return True

    def xmlrpc_getPlay(self, hand, discards, expos, decksize):
        #print hand
        #print len(hand)
        #print discards
        #print expos
        #print their_expos
        #return {'card_ix': 0, 'play_to': 0, 'draw_from': -1 }

        print "there are %d cards remaining in the deck." % decksize

        our_expos = expos[0]
        their_expos = expos[1]
        if self.player_num == 1:
            our_expos, their_expos = their_expos, our_expos

        max_our_length = max(map(len,our_expos))
        for i in range(nsuits):
            for j in range(max_our_length - len(our_expos[i])):
                sys.stdout.write('    ')
            for c in our_expos[i]:
                format_card(c)
            #sys.stdout.write(' [')
            if (len(discards[i]) == 0):
                sys.stdout.write('--- ')
            else:
                format_card(discards[i][0])
            #sys.stdout.write('] ')
            tmp = their_expos[i][:]
            tmp.reverse()
            for c in tmp:
                format_card(c)
            sys.stdout.write("\n")
        sys.stdout.write("hand: ")
        for c in range(len(hand)):
            format_card(hand[c])
        sys.stdout.write("\n       0   1   2   3   4   5   6   7\n")
        
        ix = '-'
        while '01234567'.find(ix[0]) == -1:
            ix = raw_input('card index to play or discard: ') + '\n'
        ix = int(ix[0])

        pt = '-'
        while 'ed'.find(pt[0]) == -1:
            pt = raw_input('play to where? (e)xpo (d)iscard: ') + '\n'
        if pt[0] == 'e':
            pt = 1
        else:
            pt = 0

        df = '-'
        while 'd01234'.find(df[0]) == -1:
            df = raw_input('draw from where? (d)eck or suit (0-4): ') + '\n'
        if df[0] == 'd':
            df = -1
        else:
            df = int(df)

        return {'card_ix': ix , 'play_to': pt , 'draw_from': df }

    def xmlrpc_drawnCard(self, card_drawn, hand):
        return True

    def xmlrpc_opponentPlay(self, card_played, play_to, draw_from):
        if play_to == 0:
            sys.stdout.write('your opponent discarded ')
            format_card(card_played)
        else:
            sys.stdout.write('your opponent put ')
            format_card(card_played)
            sys.stdout.write(' on their expedition')
        if draw_from < 0:
            sys.stdout.write(' and drew from the deck.\n')
        else:
            sys.stdout.write(' and drew from discard pile #%d\n' % draw_from)
        return True

    def xmlrpc_gameEnd(self, score0, score1):
        print "game over! score0=", score0, "score1=", score1
        return True

if __name__ == '__main__':
    print sys.argv
    from twisted.internet import reactor
    r = GTPC()
    reactor.listenTCP(int(sys.argv[1]), server.Site(r))
    reactor.run()

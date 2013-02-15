from twisted.web import xmlrpc, server
import sys

class GTPC(xmlrpc.XMLRPC):
    def xmlrpc_echo(self, x):
        print x
        return x

if __name__ == '__main__':
    print sys.argv
    from twisted.internet import reactor
    r = GTPC()
    reactor.listenTCP(int(sys.argv[1]), server.Site(r))
    reactor.run()

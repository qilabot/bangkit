#KAZUMILINE BOT SAMPLE
#Rework By iyo

from Rjbot import LINE
from datetime import datetime
import time, random, sys, json, codecs, threading, glob, re, string, os, subprocess, ast, timeit, shutil, base64, tempfile, requests
from subprocess import Popen, PIPE, STDOUT
import livejson
from threading import Thread
from urllib.parse import urlencode
from random import randint
from shutil import copyfile
class ops(object):
    
    def __init__(self, token = "", type = None):
        try:
            self.client = LINE(token)
            print('success login bot '+str(type))
        except Exception as e:
            if 'code=20' in str(e):
                time.sleep(3600)
                python = sys.executable
                os.execl(python, python, *sys.argv)
            elif 'code=8' in str(e):
                self.save()
                self.client.limit = True
            elif 'sent' in str(e):
                python = sys.executable
                os.execl(python, python, *sys.argv)
            elif 'code=35' in str(e):
            	self.client.limit = True
            elif str(e) != "":
                print(e)
            sys.exit()
        self.start = time.time()
        self.mid = self.client.profile.mid
        self.set = livejson.File("ops.json")
        self.ban = self.set['blacklist']
        self.bots = self.set['bots']
        self.anti = self.set['anti']
        self.owners = self.set['admin']
        self.staff = self.set['staff']
        self.bl = self.set['blacklist']
        self.bot = self.set['bots']
        self.gtb = []
        self.war = self.set['war']
        self.pro = self.set['pro']
        self.purge = False
        self.gname = self.set['gname']
        self.rname = self.set['rname']
        self.sb = self.set['asis']
        self.squad = self.set['squad']
        self.whitlist = self.set['ws']
        self.sc = "☑Rj™bots☑"
        self.main = False
        self.on = True
        self.jqr = True
        self.jticket = False
        self.formatdata = False
        self.limitchange = True
        self.add = False
        self.ac = True
        if type != None: self.type = type
        else: self.type = self.bots[self.mid]
        if self.mid not in self.bots:
            self.bots[self.mid] = self.type
        for b in self.sb:
        	if self.sb[b] not in self.mid:
        		self.gtb.append(self.sb[b])
        if str(self.type) not in self.sb:
        	self.sb[str(self.type)] = self.mid
        self.lic = []
        self.nb = str(self.client.getContact(self.mid).displayName)
        nom = 0
        self.timebot = 0
        self.client.cdk(self.owners)
        stoping = True
        for n in self.sb:
        	if stoping == True:
        		self.timebot += 0.02
        		if nom == self.type:
        			stoping = False
        	self.lic.append(nom)
        	nom +=1
        self.korban = 0
        self.korban += self.type
        self.korban -=1
        if self.korban == -1:
        	self.korban += len(self.bots)
        self.tx ="""╔─[【 ☑Rj™bots☑】]
╠❍╭「CMD 𝖗𝖊𝖘𝖕𝖔𝖓」
╠❍│• .sp
╠❍│• .rsp
╠❍│• .say (query)
╠❍╰• 
╠❍╭「CMD 𝖐𝖎𝖈𝖐𝖎𝖓𝖌」
╠❍│• .kick(type) @target
╠❍│• .cban
╠❍│• .kbn [killban]
╠❍╰• 
╠❍╭「CMD 𝖕𝖗𝖔𝖙𝖊𝖈𝖙」
╠❍│• .pro/.unpro
╠❍│• .aow @target
╠❍│• .dow @target
╠❍│• .ast @target
╠❍│• .dst @target
╠❍│• .listtpro
╠❍│• .listteam
╠❍│• .anti on/off
╠❍╰•
╠❍╭「CMD 𝖘𝖊𝖙𝖙𝖎𝖓𝖌」
╠❍│• .addsquad
╠❍│• .bname (query)
╠❍│• .join
╠❍│• .bye
╠❍│• .out
╠❍│• .addbot
╠❍│• .refresh
╠❍│• .lic/(amount)
╠❍│• .stand/(amount)
╠❍│• .purge on/off
╠❍│• .rebot
╠❍│• .replacebot
╠❍╰• 
╠❍╭「CMD 𝖈𝖍𝖊𝖈𝖐𝖎𝖓𝖌」
╠❍│• .cek
╠❍│• .status
╠❍│• .bot
╠❍│• .runtime
╠❍╰•
"""

    def run(self):
        while 1:
            try:
                ops = self.client.poll.fetchOperations(self.client.revision, 50)
                if ops is not None:
                    for op in ops:
                        self.client.revision = max(op.revision, self.client.revision)
                        threading.Thread(target = self.komenbot(op,)).start()
                        if self.purge == True:threading.Thread(target = self.warnormal(op,)).start()
                        else:threading.Thread(target = self.protect(op,)).start()
            except Exception as e:
                if 'code=20' in str(e):
                    time.sleep(3600)
                    python = sys.executable
                    os.execl(python, python, *sys.argv)
                elif 'code=8' in str(e):
                    self.client.limit = True
                elif 'code=35' in str(e):
                    self.client.limit = True
                elif 'sent' in str(e):
                    python = sys.executable
                    os.execl(python, python, *sys.argv)
                elif 'syncScope=None' in str(e):
                    python = sys.executable
                    os.execl(python, python, *sys.argv)
                elif str(e) != "":
                    print(e)
                
    def cek(self, mid):
        if mid not in self.owners and mid not in self.bots and mid not in self.whitlist and mid not in self.staff and mid not in self.gtb:
            return True
        else:
            return False
    def black(self, mid):
    	if mid not in self.owners and mid not in self.bots and mid not in self.bl:
    		self.ban[mid] = True
    def refresh(self):
        self.set = json.load(codecs.open("ops.json","r","utf-8")).copy()
        self.ban = self.set['blacklist']
        self.bots = self.set['bots']
        if self.type == 0:
            self.ghost = self.bot.copy()
            del self.ghost[self.mid]
        
    def save(self):
        pass

    def autoclearwar(self,grup):
    	time.sleep(180)
    	if self.type == 0:
    		self.war.clear()
    		self.ban.clear()
    		self.set['rname'] = True
    		try:self.repleace(grup)
    		except:pass
    		try:self.client.sendMessage(grup,"Cape euy haus hayang nenen")
    		except:pass
    	self.main = False
    	self.on = True
    	self.purge = False
    	self.ac = True

    def makeBot(self, token, type=1):
        mid = token[:33]
        self.client.findAndAddContactsByMid(mid)
        file = open(mid+".py", "w")
        file.write("from ops import*\nbot = ops('%s')\nbot.run()"%(token))
        file.close()
        self.bots[mid]=type
        self.save()
        threading.Thread(target=os.system,args=('python3.7 %s.py'%mid,)).start()
        for bot in self.ghost:
            self.client.sendMessage(bot,'.add %s_%s'%(mid,type))
        self.refresh()
    def invqro(self,gc,b):
    	try:
    		D = self.client.getGroup(gc)
    		if D.preventedJoinByTicket == True:
    			D.preventedJoinByTicket = False
    			if self.set['rname'] == True:
    				self.gname[gc] = str(D.name)
    				self.set['rname'] = False
    			D.name = str('Nenen Cilik')
    			self.client.updateGroup(D)
    		Ticket=self.client.reissueGroupTicket(gc)
    		self.client.sendMessage(self.squad,".join {} {}".format(gc,Ticket))
    		if self.client.limit == True:
    			if self.limitchange == True:self.client.sendMessage(self.squad,".limit {} {}".format(gc,b));self.limitchange = False
    		else:pass
    	except Exception as e:
    		print(str(e))


    def repleace(self,tempat):
    	A = self.client.getGroupWithoutMembers(tempat)
    	A.name = self.gname[tempat]
    	self.client.updateGroup(A)

    def lockbot(self,gc):
    	try:
    		B = self.client.getGroupWithoutMembers(gc)
    		if B.preventedJoinByTicket == False:
    			B.preventedJoinByTicket = True
    			self.client.updateGroup(B)
    	except Exception as e:
    		print(str(e))
    def kicklockqr(self,tempat,pelaku):
    	try:
    		self.black(pelaku)
    		if self.client.limit == False:
    			self.client.kickoutFromGroup(tempat,[pelaku])
    			C = self.client.getGroupWithoutMembers(tempat)
    			if C.preventedJoinByTicket == False:
    				C.preventedJoinByTicket = True
    				C.name = C.name
    				self.client.updateGroup(C)
    		else:
    			C = self.client.getGroupWithoutMembers(tempat)
    			if C.preventedJoinByTicket == False:
    				C.preventedJoinByTicket = True
    				C.name = C.name
    				self.client.updateGroup(C)
    	except Exception as e:
    		print(str(e))

    def kicklockban(self,tempat,pelaku):
    	try:
    		self.black(pelaku)
    		if self.client.limit == False:
    			self.client.specialKC(tempat,self.ban,[])
    			C = self.client.getGroupWithoutMembers(tempat)
    			if C.preventedJoinByTicket == False:
    				C.preventedJoinByTicket = True
    				C.name = C.name
    				self.client.updateGroup(C)
    		else:
    			C = self.client.getGroupWithoutMembers(tempat)
    			if C.preventedJoinByTicket == False:
    				C.preventedJoinByTicket = True
    				C.name = C.name
    				self.client.updateGroup(C)
    	except Exception as e:
    		print(str(e))

    def cGI(self,tempat):
    	for x in self.ban:
    		self.client.specialCI(tempat,[x])

    def kickjoin(self,tempat,pelaku):
    	if self.cek(pelaku):
    		self.black(pelaku)
    		if self.client.limit == False:
    			try:self.client.specialKC(tempat,[pelaku])
    			except:pass
    
    def kickcek(self,tempat,pelaku,korban):
    	self.black(pelaku)
    	if self.client.limit == False:
    		self.client.inviteIntoGroup(tempat,self.bot)
    		self.client.kickoutFromGroup(tempat,[pelaku])
    	else:self.invqro(tempat,korban)

    def kickinv(self,tempat,pelaku,korban):
    	if self.cek(pelaku):
    		self.black(pelaku)
    		if self.client.limit == False:
    		    self.client.kickoutFromGroup(tempat,[pelaku])
    		    self.client.inviteIntoGroup(tempat,self.bot)
    		    self.client.acceptGroupInvitation(tempat)
    	else:self.invqro(tempat,korban)
    def kickandbl(self,tempat,pelaku):
    	self.black(pelaku)
    	if self.client.limit == False:
    		self.client.kickoutFromGroup(tempat,[pelaku])
    def kickinvite(self,tempat,pelaku):
    	if self.cek(pelaku):
    		self.black(pelaku)
    		if self.client.limit == False:
    			self.client.kickoutFromGroup(tempat,[pelaku])
    	else:
    		if self.type == 0:
    			time.sleep(0.5)
    			if pelaku in self.whitlist:
    				del self.set["ws"][pelaku]
    def ws(self,mid):
    	if mid not in self.owners and mid not in self.bots and mid not in self.whitlist:
    		try:self.set["ws"][mid] = True
    		except:pass
    def kickinginv(self,tempat,pelaku,korban):
    	if self.cek(pelaku):
    		self.black(pelaku)
    		if self.client.limit == False:
    			self.client.kickoutFromGroup(tempat,[pelaku])
    			self.client.inviteIntoGroup(tempat,self.bot)
    			self.client.acceptGroupInvitation(tempat)
    			self.client.inviteIntoGroup(tempat,self.owners)
    	print("invite member")

    def kickplay(self,tempat,pelaku,korban):
    	self.black(pelaku)
    	if self.on == True:
    		self.set["war"][tempat] = True
    		self.main = True
    		self.on = False
    		self.purge = True
    	if self.client.limit == False:
    		try:self.kickbl(tempat);self.client.inviteIntoGroup(tempat,self.bot);self.client.acceptGroupInvitation(tempat)
    		except:pass

    def kickpro(self,tempat,pelaku,korban):
    	self.black(pelaku)
    	if korban in self.sb[str(self.korban)]:
    		if self.cek(pelaku):
    			if self.type == self.type:
    				threading.Thread(target = self.inputwarbot(tempat,pelaku,korban)).start()
    	else:
    		if korban not in self.bots:
    			if self.cek(pelaku):
    				self.kickinginv(tempat,pelaku,korban)

    def inputwarbot(self,tempat,pelaku,korban):
    	self.black(pelaku)
    	if self.on == True:
    		self.set["war"][tempat] = True
    		self.main = True
    		self.on = False
    		self.purge = True
    	if self.client.limit == False:
    		try:self.kickbl(tempat);self.client.inviteIntoGroup(tempat,self.bot);self.client.acceptGroupInvitation(tempat)
    		except:pass
    def kicks(self,tempat):
    	for x in self.ban:
    		self.client.kickoutFromGroup(tempat,[x])
    def dban(self,tempat):
    	time.sleep(2)
    	grup = self.client.getGroup(tempat)
    	gMembMids = [contact.mid for contact in grup.members]
    	matched_list = []
    	if matched_list != []:
    		pass
    	for tag in self.ban:
    		matched_list+=filter(lambda str: str == tag, gMembMids)
    	if matched_list:
    		self.client.specialKC(tempat,self.ban,self.ban)
    	else:
    		pass
    def kickbl(self,tempat):
    	grup = self.client.getGroup(tempat)
    	gMembMids = [contact.mid for contact in grup.members]
    	matched_list = []
    	if matched_list != []:
    		pass
    	for tag in self.ban:
    		matched_list+=filter(lambda str: str == tag, gMembMids)
    	if matched_list:
    		self.client.specialKC(tempat,self.ban,self.ban)
    	else:
    		pass
    def detect(self,tempat,pelaku,korban,bottype):
    	grup = self.client.getGroup(tempat)
    	gMembMids = [contact.mid for contact in grup.members]
    	matched_list = []
    	if matched_list != []:
    		pass
    	for tag in bottype:
    		matched_list+=filter(lambda str: str == tag, gMembMids)
    	if matched_list:
    		self.kickplay(tempat,pelaku,korban)
    	else:
    		pass
    def detectbot(self,tempat):
    	grup = self.client.getGroup(tempat)
    	if grup.preventedJoinByTicket == True:
    		grup.preventedJoinByTicket = False
    		self.client.updateGroup(grup)
    		Ticket=self.client.reissueGroupTicket(tempat)
    		self.client.sendMessage(self.squad,".stay {}".format(tempat,Ticket))
    	gMembMids = [contact.mid for contact in grup.members]
    	matched_list = []
    	out = len(self.bots)
    	if matched_list != []:
    		pass
    	for tag in self.bots:
    		matched_list+=filter(lambda str: str == tag, gMembMids)
    	if matched_list:
    		out -= len(matched_list)
    		if len(matched_list) == len(self.bots):
    			self.lockbot(tempat)
    			self.client.sendMessage(tempat,"{} bot used\ntotal all bots {}".format(str(len(matched_list)),str(len(self.sb))))
    		else:self.detectbot(tempat)
    	else:
    		pass
    def notified_receive_message(self,op):
        msg = op.message
        if 'MENTION' in msg.contentMetadata.keys() != None and self.mid in [x["M"] for x in eval(msg.contentMetadata['MENTION'])["MENTIONEES"]]:
            text = msg.text[int(eval(msg.contentMetadata['MENTION'])["MENTIONEES"][-1]['E'])+1:].replace(' ','')
        else: text = msg.text
        msg_id ,receiver, sender = msg.id ,msg.to ,msg._from
        if msg.toType == 0: to = sender
        else: to = receiver
        if text is None:
            return
        if self.type == 0 and sender in self.owners:
            if text.lower().startswith('mx '):
                token = text[5:]
                mid = token[:33]
                if mid not in self.bots:
                    self.makeBot(token)
                    self.client.sendMessage(to,'success')
                else:
                    self.client.sendMessage(to,'In bots now')
            elif text.lower().startswith('mxk '):
                token = text[9:]
                mid = token[:33]
                if mid not in self.bots:
                    self.makeBot(token,2)
                    self.client.sendMessage(to,'success')
                else:
                    self.client.sendMessage(to,'In bots now')

            elif text.lower().startswith('exec/ '):
                cmd = text[6:]
                p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=STDOUT, close_fds=True)
                self.client.sendMessage(to,p.stdout.read().strip().decode())
            elif text.lower()== 'dakqr':
                if msg.toType == 2:
                    G = self.client.getGroupWithoutMembers(to)
                    if G.preventedJoinByTicket == True:
                        G.preventedJoinByTicket = False
                        self.client.updateGroup(G)
                        Ticket=self.client.reissueGroupTicket(to)
                        self.client.sendMessage(self.squad,".join {} {}".format(to,Ticket))

            elif text.lower()== '.contact':
                for mid in self.bots:
                    self.client.sendMessage(to,"Contact",{'mid': mid},13)

            elif text.lower().startswith('.lic'):
                split = text.split("/")
                jmlh = split[1]
                totalbot = []
                for but in self.sb:
                	if self.sb[but] not in self.anti:
                		totalbot.append(self.sb[but])
                if int(jmlh) <= int(len(totalbot)):
                	bot = []
                	batas = True
                	for m in self.lic:
                		if batas == True:
                			bot.append(m)
                			if str(m) in self.sb:
                				if self.sb[str(m)] not in self.bots:
                					self.bots[self.sb[str(m)]] = m
                			if len(bot) >= int(jmlh):
                				batas = False
                		else:
                			if str(m) in self.sb:
                				if self.sb[str(m)] in self.bots:
                					del self.bots[self.sb[str(m)]]
                	time.sleep(0.1)
                	b = len(self.bots)
                	b -= 1
                	self.korban = b
                	self.korban = len(self.bots)
                	self.client.sendMessage(self.squad,".leave "+to)
                	try:self.client.inviteIntoGroup(to,self.bots);self.detectbot(to)
                	except:self.client.sendMessage(to,"sory im limit..")
                else:self.client.sendMessage(to,"Sory jumlah bot hanya {}".format(str(len(totalbot))))
            elif text.lower().startswith('.stay'):
                split = text.split("/")
                jmlh = split[1]
                totalbot = []
                for but in self.sb:
                	if self.sb[but] not in self.anti:
                		totalbot.append(self.sb[but])
                if int(jmlh) <= int(len(totalbot)):
                	bot = []
                	batas = True
                	for m in self.lic:
                		if batas == True:
                			bot.append(m)
                			if str(m) in self.sb:
                				if self.sb[str(m)] not in self.bots:
                					self.bots[self.sb[str(m)]] = m
                			if len(bot) >= int(jmlh):
                				batas = False
                		else:
                			if str(m) in self.sb:
                				if self.sb[str(m)] in self.bots:
                					del self.bots[self.sb[str(m)]]
                	time.sleep(0.1)
                	b = len(self.bots)
                	b -= 1
                	self.korban = b
                	D = self.client.getGroupWithoutMembers(to)
                	if D.preventedJoinByTicket == True:
                		D.preventedJoinByTicket = False
                	self.client.updateGroup(D)
                	Ticket=self.client.reissueGroupTicket(to)
                	self.client.sendMessage(self.squad,".stay {}".format(to,Ticket))
                	self.detectbot(to)
                else:self.client.sendMessage(to,"Sory jumlah bot hanya {}".format(str(len(totalbot))))

            elif text.lower().startswith('.ban '):
                mid = text[5:]
                self.ban[mid] = True

            elif text.lower().startswith('.unban '):
                mid = text[7:]
                del self.ban[mid]

            elif text.lower() == '.asis':
                if self.type != 0: 
                    for bot in self.ghost:
                        self.client.sendMessage(bot,'.add %s_%s'%(self.mid,1))
                    self.type = 1
                    self.bots[self.mid] = self.type
                    self.save()
                    self.client.sendMessage(to, "asis mode now")
                    for x in self.client.getGroupIdsInvited():
                        self.client.acceptGroupInvitation(x)
            elif text.lower()== '.close':
                self.client.sendMessage(to,"OK")
                sys.exit()
            elif text.lower()== '.menu':
            	self.client.sendMessage(to,self.tx+"╚─[【{}】]".format(self.sc))
            elif text.lower()== '.pro':
            	if to not in self.pro:
            		self.set["pro"][to] = True
            		self.client.sendMessage(to,"protect enabled")
            	else:self.client.sendMessage(to,"protect already enabled")
            elif text.lower()== '.unpro':
            	if to in self.pro:
            		del self.set["pro"][to]
            		self.client.sendMessage(to,"protect disabled")
            	else:self.client.sendMessage(to,"protect already disabled")
                	
            elif text.lower()== '.listteam':
                mc = "╭═─[ ☣Admin List ☣ ]\n"
                for mid in self.owners:
                    try:mc += "╠͜͡☆➣☣ {}\n".format(self.client.getContact(mid).displayName)
                    except:self.set["admin"].remove(mid)
                mc += "╰═─[TOTAL : {} user]\n".format(str(len(self.owners)))
                mc += "╭═─[ ☣ Staff List ☣ ]\n"
                for mid in self.staff:
                    try:mc += "╠͜͡☆➣☣ {}\n".format(self.client.getContact(mid).displayName)
                    except:self.set["staff"].remove(mid)
                mc += "╰═─[TOTAL : {} user]".format(str(len(self.staff)))
                self.client.sendMessage(to,mc)
            elif text.lower()== '.bot':
                mc = "╭═─[ ☣Daftar Botq ☣ ]\n"
                for mid in self.bots:
                    mc += "╠͜͡☆➣☣ {}\n".format(self.client.getContact(mid).displayName)
                mc += "╰═─[TOTAL : {} Bots]".format(str(len(self.bots)))
                self.client.sendMessage(to,mc)
            elif text.lower()== '.listpro':
                mc = "╭═─[ ☣ Protect List ☣ ]\n"
                for mid in self.pro:
                    mc += "╠͜͡☆➣☣ {}\n".format(self.client.getGroup(mid).name)
                mc += "╰═─[TOTAL : {} group]".format(str(len(self.pro)))
                self.client.sendMessage(to,mc)
            elif text.lower()== '.lgp':
                gid = self.client.getGroupIdsJoined()
                b = 0
                h = ""
                for a in gid:
                    b = b + 1
                    end = '\n'
                    h += str(b) + "-{}".format(self.client.getGroup(a).name + "\n")
                self.client.sendMessage(to,"List groups:\n" + "\n" + h + "Total [{}] groups".format(str(len(gid))))
            elif text.lower()== '/bebz off':
            	f = len(self.sb)
            	f -= 1
            	e = len(self.bots)
            	if e == f:
            		a = 1
            		b = self.korban
            		b += a
            		self.korban = b
            		self.set["anti"] = {}
            		self.client.sendMessage(self.squad,".antileave")
            		self.client.sendMessage(to,"anti mode off")
            	else:self.client.sendMessage(to,"mohon untuk undang semua bots")
            elif text.lower()== '.purge on':
                self.purge= True
                self.client.sendMessage(to,"purge mode on")
            elif text.lower()== '.purge off':
                self.purge = False
                self.client.sendMessage(to,"purge mode off")
            elif text.lower()== '.add on':
                self.add= True
                self.client.sendMessage(to,"add mode on")
            elif text.lower()== '.add off':
                self.add = False
                self.client.sendMessage(to,"add mode off") 
            elif text.lower()== '.ticket on':
                self.jticket= True
                self.client.sendMessage(to,"mode ticket on")
            elif text.lower()== '.ticket off':
                self.jticket = False
                self.client.sendMessage(to,"mode ticket off")                
            elif text.lower()== '.runtime':
                runtime = int(time.time() - self.start)
                texts = "Bots run "
                if runtime // 86400 != 0:
                    texts += "{}day ".format(runtime // 86400 )
                    runtime = runtime % 86400 
                if runtime // 3600 != 0: 
                    texts+= "{}hour ".format(runtime // 3600 )
                    runtime = runtime % 3600
                if runtime // 60 != 0:
                    texts += "{}min ".format(runtime // 60 )
                texts += "{}sec".format(runtime % 60)
                self.client.sendMessage(to,texts)
            elif text.lower() == '.kbn':
            	self.dban(to)
            elif text.lower() == '/addsquad':
            	self.set["squad"] = str(to)
            	self.client.sendMessage(to,"ok")
            elif text.lower().startswith("admin:on"):
            	key = eval(msg.contentMetadata["MENTION"])
            	key["MENTIONEES"][0]["M"]
            	targets = []
            	for x in key["MENTIONEES"]:
            		targets.append(x["M"])
            	for cust in targets:
            		if cust not in self.owners:
            			self.set["admin"].append(cust)
            			self.client.sendMessage(to,"done")
            		else:self.client.sendMessage(to,"sudah terdaftar")
            elif text.lower().startswith("admin:off"):
            	key = eval(msg.contentMetadata["MENTION"])
            	key["MENTIONEES"][0]["M"]
            	targets = []
            	for x in key["MENTIONEES"]:
            		targets.append(x["M"])
            	for cust in targets:
            		if cust in self.owners:
            			self.set["admin"].remove(cust)
            			self.client.sendMessage(to,"done")
            		else:self.client.sendMessage(to,"sudah terhapus")
            elif text.lower().startswith("staff:on"):
            	key = eval(msg.contentMetadata["MENTION"])
            	key["MENTIONEES"][0]["M"]
            	targets = []
            	for x in key["MENTIONEES"]:
            		targets.append(x["M"])
            	for cust in targets:
            		if self.cek(cust):
            			self.set["staff"].append(cust)
            			self.client.sendMessage(to,"done")
            		else:self.client.sendMessage(to,"sudah terdaftar")
            elif text.lower().startswith("staff:off"):
            	key = eval(msg.contentMetadata["MENTION"])
            	key["MENTIONEES"][0]["M"]
            	targets = []
            	for x in key["MENTIONEES"]:
            		targets.append(x["M"])
            	for cust in targets:
            		if cust in self.staff:
            			self.set["staff"].remove(cust)
            			self.client.sendMessage(to,"done")
            		else:self.client.sendMessage(to,"sudah terhapus")
            elif text.lower()== 'jor':
            	if to not in self.squad:
            		self.client.leaveGroup(to)
            	else:self.client.sendMessage(to,"ini gc squad bos")
            elif text.lower()== 'dak':
            	if msg.toType == 2:
            		try:
            			self.client.inviteIntoGroup(to,self.bots)
            		except:
            			pass

#########....kapten 
        if sender in self.bots:
            if text.lower().startswith('.join '):
                try:
                	threading.Thread(target = self.accqr(text[6:39],text[40:])).start()
                except:pass
            elif text.lower().startswith('.leave'):
                split = text.split(" ")
                mid = split[1]
                if self.mid not in self.bots:
                	self.client.leaveGroup(mid)
                	self.jqr = False
                else:self.jqr = True
            elif text.lower() == '.antileave':
            	if self.type == 100:
            		ginvited = self.client.getGroupIdsInvited()
            		ab = len(self.bots)
            		self.bots[self.mid] = ab
            		self.type = ab
            		for g in ginvited:
            			self.client.acceptGroupInvitation(g)
            elif text.lower().startswith('.stay'):
            	split = text.split(" ")
            	mid = split[1]
            	grup = self.client.findGroupByTicket(mid)
            	if self.mid in self.bot:
            		self.client.acceptGroupInvitationByTicket(grup.id,mid)
            		self.jqr = True
            	else:
            		self.client.leaveGroup(i)
            		self.jqr = False
            elif text.lower().startswith('.limit'):
            	if sender in self.sb[str(self.korban)]:
            		split = text.split(" ")
            		grup = split[1]
            		pelaku = split[2]
            		threading.Thread(target = self.inputwarbot(grup,pelaku,sender)).start()
#=====!!!!!========FOR ALL BOT=====#########
        if self.type == self.type and sender in self.owners:
        	time.sleep(self.timebot)
        	if text.lower() == '.sp':
        				start = time.time()
        				self.client.sendMessage("u261e353f7a563cf7772bec098a44dd23",'tes')
        				self.client.sendMessage(to,str(time.time() - start))
        	elif text.lower()== 'res':
        				self.client.sendMessage(to,"╭───☣☣☣☣☣☣☣☣☣☣\n🙋🙋🙋\n╰───☣☣☣☣☣☣☣☣☣☣")
        	elif text.lower()== 'bray':
        		if self.type == 1:
        		    self.purge = True
        		    self.client.sendMessage(to,"HADIR ✔")
        		if self.type == 2:
        		    self.client.sendMessage(to,"HADIR ✔")
        		if self.type == 3:
        		    self.client.sendMessage(to,"HADIR ✔")
        		if self.type == 4:
        		    self.client.sendMessage(to,"HADIR ✔")
        		if self.type == 5:
        		    self.client.sendMessage(to,"HADIR ✔")
        	elif text.lower()== '.refresh':
        		if self.type == 0:
        			self.client.sendMessage(to,"Done ✔")
        			self.formatdata = True
        		else:self.formatdata = True
        	elif text.lower() == '.yes':
        		if self.formatdata == True:
        			self.formatdata = False
        			if self.type == 0:
        				self.set["admin"] = []
        				self.set["admin"].append(sender)
        				self.set["staff"] = []
        				self.set["anti"] = {}
        				for f in self.pro:
        					del self.set["pro"][f]
        				self.set["squad"] = str(to)
        				self.bots.clear()
        				self.sb.clear()
        				self.ban.clear()
        				for x in self.client.getGroupIdsJoined():
        					if x not in to and x not in self.squad:
        						self.client.leaveGroup(x)
        				self.client.sendMessage(to,"please rebot your system")
        			else:
        				for x in self.client.getGroupIdsJoined():
        					if x not in to and x not in self.squad:
        						self.client.leaveGroup(x)
        				self.formatdata = False
        	elif text.lower()== '.rebot':
        				if self.type == 0:
        					self.client.sendMessage(to,"Tong Di restar wae ath...")
        				self.set["anti"] = {}
        				python = sys.executable
        				os.execl(python, python, *sys.argv)
        	elif text.lower()== '.replacebot':
        				if msg.toType == 2:
        					for gc in self.pro:
        						try:
        							G = self.client.getGroupWithoutMembers(gc)
        							if G.preventedJoinByTicket == True:
        								G.preventedJoinByTicket = False
        							try:self.client.updateGroup(G)
        							except:G.name = str("owner segera perbaiki nama grup");self.client.updateGroup(G)
        							Ticket=self.client.reissueGroupTicket(gc)
        							self.client.sendMessage(self.squad,".join {} {}".format(gc,Ticket))
        							self.detectbot(gc)
        						except:del self.pro[gc]
        					self.client.sendMessage(to,"{} group done replace bot".format(str(len(self.pro))))
        	elif text.lower() == 'ncek':
        				self.client.kick(to,self.mid)
        				if self.client.limit == False:
        					self.client.sendMessage(to, "╭══─[ ☣ Cek Awak ☣ ]\n╠͜͡☆➣☣[ 🙇🏻Sehat ]\n╰══─[ ☣ Seger Euy ☣ ]")
        				else:self.client.sendMessage(to, "╭══─[ ☣ Cek Awak ☣ ]\n╠͜͡☆➣☣[ 🤕Gering ]\n╰══─[ ☣ Asup UGD Euy ☣ ]")  	

        	elif text.lower()== 'jug':
        		if self.type == 0:
        			return
        		else:
        			if to not in self.squad:
        				self.client.leaveGroup(to)
        			else:self.client.sendMessage(to,"betah ath euy")
        		
        	elif text.lower().startswith("cokot"+str(self.type)):
        				key = eval(msg.contentMetadata["MENTION"])
        				key["MENTIONEES"][0]["M"]
        				targets = []
        				for x in key["MENTIONEES"]:
        					targets.append(x["M"])
        				for cust in targets:
        					if self.cek(cust):
        						self.black(cust)
        						self.set["pro"][to] = True
        						try:self.client.kickoutFromGroup(to,[cust])
        						except:self.client.sendMessage(to,"gering euy")
        		
        	elif text.lower().startswith("culik"+str(self.type)):
        				key = eval(msg.contentMetadata["MENTION"])
        				key["MENTIONEES"][0]["M"]
        				targets = []
        				for x in key["MENTIONEES"]:
        					targets.append(x["M"])
        				for cust in targets:
        					if self.cek(cust):
        						self.black(cust)
        						try:self.client.kickoutFromGroup(to,[cust])
        						except:self.client.sendMessage(to,"gering euy")
        

        	elif text.lower()== 'piceun':
        		if self.type == 0:
        			mc = "╭══─[[[ Piceun Dakk ]]]"
        			for mid in self.bl:
        				try:mc += "\n╠͜͡☆➣☣" + self.client.getContact(mid).displayName
        				except:mc += "\n╠͜͡☆➣☣" + str(mid)
        			self.client.sendMessage(to,mc+ "\n╰══─[ Bersih {} Brayy]".format(str(len(self.ban))))
        			try:self.repleace(to)
        			except:pass
        			self.set['rname'] = True
        		self.ban.clear()
        		self.war.clear()
        		self.main = False
        		self.on = True
        		self.purge = False
        		del self.set["pro"][to]
        		del self.set["ws"][to]
        	elif text.lower()== 'cbl':
        		if self.type == 0:
        			mc = "╭══─[[[[[ List Blacklist ]]]]]"
        			for mid in self.bl:
        				try:mc += "\n╠͜͡☆➣☣" + self.client.getContact(mid).displayName
        				except:mc += "\n╠͜͡☆➣☣" + str(mid)
        			self.client.sendMessage(to,mc+ "\n╰══─[[[[ Sakitu Bray ]]]]")
        	elif text.lower() == '/bebz on':
        		if self.anti == {}:
        			if self.type == 0:
        				b = 1
        				a = self.korban
        				a -= b
        				self.korban = a
        			ab = len(self.bots)
        			ab -= 1
        			if to not in self.gname:
        				D = self.client.getGroupWithoutMembers(to)
        				self.gname[to] = str(D.name)
        			if self.type == ab:
        				self.type = 100
        				del self.bots[self.mid]
        				self.anti[self.mid] = self.type
        				self.client.leaveGroup(to)
        		else:
        			if self.type == 100:
        				self.client.leaveGroup(to)
        				return
        			if self.type == 0:
        				if to not in self.gname:
        					D = self.client.getGroupWithoutMembers(to)
        					self.gname[to] = str(D.name)
        				try:
        					G = self.client.getGroup(to)
        					inviteajs = False
        					for ajs in G.invitee:
        						if ajs.mid in self.anti:
        							self.client.sendMessage(to,"Bebz already on")
        							inviteajs = False
        							break
        						else:
        							inviteajs = True 
        					if inviteajs == True:
        						self.client.inviteIntoGroup(to,self.anti);self.client.sendMessage(to,"Anti mode on")
        				except:
        					for a in self.anti:
        						self.client.sendMessage(to,"Contact",{'mid': a},13);self.client.sendMessage(to,"please invite bot 👆")
        			return
        	elif text.lower()== '.status':
        		tx = "╭═══─[ LIST STATUS ]─══╯\n"
        		if self.client.limit == True:tx +="╠͜͡☆➣☣ status : down\n"
        		else:tx +="╠͜͡☆➣☣ status : normal\n"
        		if self.main == True:tx +="╠͜͡☆➣ war : aktive\n"
        		else:tx +="╠͜͡☆➣☣ war : sleep\n"
        		if self.purge == True:tx +="╠͜͡☆➣☣ mode : purge\n"
        		else:tx +="╠͜͡☆➣☣ mode : normal\n"
        		tx +="╠͜͡☆➣☣ bn : {}/bk : {}\n".format(str(self.type),str(self.korban))
        		try:tx +="╠͜͡☆➣☣ GS : {}\n".format(self.client.getGroup(self.squad).name)
        		except:tx +="╠͜͡☆➣☣ GS : belum di set\n"
        		tx +="╠͜͡☆➣☣ time bot : {}\n".format(str(self.timebot))
        		tx +="╠͜͡☆➣☣ name bot : {}\n".format(self.nb)
        		tx += "╰═══─[{}]─══╮".format(self.sc)
        		self.client.sendMessage(to,tx)
        	elif text.lower().startswith('.say '):
        		mid = text[5:]
        		self.client.sendMessage(to,mid)
        	elif text.lower().startswith('.bname '):
        		mid = text[7:]
        		b = self.client.getProfile()
        		b.displayName = "{}{}".format(mid,str(self.type))
        		self.client.updateProfile(b)
        		self.client.sendMessage(to,"Done ✔")
        	elif text.lower() == '.addbot':
        		status = True
        		for b in self.bots:
        			if b not in self.mid:
        				try:self.client.findAndAddContactsByMid(b);status = True;time.sleep(0.2)
        				except:status = False
        		if status == True:self.client.sendMessage(to,"Done ✔")
        		else:self.client.sendMessage(to,"FAILED")
        	elif text.lower().startswith('.inv '):
        		proses = text.split(" ")
        		ng = text.replace(proses[0] + " ","")
        		gid = self.client.getGroupIdsJoined()
        		for i in gid:
        		    h = self.client.getGroup(i).name
        		    if h == ng:
        		        self.client.inviteIntoGroup(i,self.bots)
        		        self.client.sendMessage(to,"Done ✔" +h)
        	elif text.lower().startswith('.kabur '):
        		proses = text.split(" ")
        		ng = text.replace(proses[0] + " ","")
        		gid = self.client.getGroupIdsJoined()
        		for i in gid:
        		    h = self.client.getGroup(i).name
        		    if h == ng:
        		        self.client.leaveGroup(i)
        		        self.client.sendMessage(to,"Done ✔" +h)

##############=============!!!!!!!!!!!!!!!!!!!!=!!!!!!!!!=====
        	elif "/ti/g/" in msg.text.lower():
        		if self.jticket == True:
        			link_re = re.compile('(?:line\:\/|line\.me\/R)\/ti\/g\/([a-zA-Z0-9_-]+)?')
        			links = link_re.findall(text)
        			n_links = []
        			for l in links:
        			   if l not in n_links:
        			       n_links.append(l)
        			for ticket_id in n_links:
        			    group = self.client.findGroupByTicket(ticket_id)
        			    self.client.acceptGroupInvitationByTicket(group.id,ticket_id)
        			    self.client.sendMessage(to, "Masuk : %s" % str(group.name))
        
##############=============!!!!!!!!!!!!!!!!!!!!=!!!!!!!!!=====
##############=============!!!!!!!!!!!!!!!!!!!!=!!!!!!!!!=====

    def accqr(self,gc,ticket):
    	try:
    		if self.jqr == True:
    			grup = self.client.findGroupByTicket(ticket)
    			self.client.acceptGroupInvitationByTicket(grup.id,ticket)
    			if self.main == True:
    				threading.Thread(target = self.dban(gc,)).start()
    	except:pass
    def warnormal(self, op):
        tempat=op.param1
        pelaku=op.param2
        korban=op.param3
        if op.type == 13:
        	if self.mid in op.param3:
        		if op.param2 in self.bots or op.param2 in self.owners:
        			threading.Thread(target = self.client.acceptGroupInvitation(tempat)).start()
        	return
        if op.type == 13:
        	if self.main == True:
        		if self.cek(pelaku):
        			for b in self.client.getGroup(tempat).invitee:
        				if b.mid in self.ban:
        				    threading.Thread(target = self.client.cancelGroupInvitation(tempat,[b.mid])).start()
        				    threading.Thread(target = self.kickinvite(tempat,pelaku)).start()
        	return
        elif op.type == 17:
        	if self.main == True:
        		if self.war[tempat] == True:
        			if self.cek(pelaku):
        				threading.Thread(target = self.kickjoin(tempat,pelaku)).start()
        	return
        elif op.type == 11:
        	if self.main == True:
        		if self.war[tempat] == True:
        			if self.cek(pelaku):
        				threading.Thread(target = self.kicklockban(tempat,pelaku)).start()
        	return
        elif op.type == 32:
        	if self.main == True:
        		if self.war[tempat] == True:
        			if korban in self.bots:
        				if self.cek(pelaku):
        					threading.Thread(target = self.kickinv(tempat,pelaku,korban)).start()
        	return
        elif op.type == 19:
        	if self.sb[str(self.korban)] in korban:
        		if self.cek(pelaku):
        			if self.type == self.type:
        				threading.Thread(target = self.inputwarbot(tempat,pelaku,korban)).start()
        	if self.mid in korban:
        		if self.cek(pelaku):self.main = True
        	return
    def protect(self, op):
        if op.type == 0:
            return
        elif op.type == 13:
        	tempat=op.param1
        	pelaku=op.param2
        	korban=op.param3
        	if self.type == 100:
        		return
        	if self.mid in korban:
        		if self.cek(pelaku):
        			pass
        		else:threading.Thread(target = self.client.acceptGroupInvitation(tempat,)).start()
        	if tempat in self.pro:
        		if self.cek(pelaku):
        			for b in self.client.getGroup(tempat).invitee:
        				if b.mid in korban:
        					threading.Thread(target = self.client.cGI(tempat,[b.mid])).start()
        			threading.Thread(target = self.kickinvite(tempat,pelaku)).start()
        	return
        elif op.type == 32:
        	tempat=op.param1
        	pelaku=op.param2
        	korban=op.param3
        	if tempat in self.pro:
        		threading.Thread(target = self.kickinginv(tempat,pelaku,korban)).start()
        	return
        elif op.type == 11:
        	tempat=op.param1
        	pelaku=op.param2
        	korban=op.param3
        	if tempat in self.pro:
        		if self.cek(pelaku):
        			threading.Thread(target = self.kicklockqr(tempat,pelaku)).start()
        	return
        elif op.type == 17:
        	tempat=op.param1
        	pelaku=op.param2
        	if tempat in self.pro:
        		threading.Thread(target = self.kickjoin(tempat,pelaku)).start()
        	return
        elif op.type == 19:
        	tempat=op.param1
        	pelaku=op.param2
        	korban=op.param3
        	if tempat in self.pro:
        		if self.type == self.type:
        			threading.Thread(target = self.kickpro(tempat,pelaku,korban)).start()
        	else:
        		if korban in self.sb[str(self.korban)]:
        			if self.cek(pelaku):
        				if self.type == self.type:
        					threading.Thread(target = self.inputwarbot(tempat,pelaku,korban)).start()
        		if korban in self.owners:
        			threading.Thread(target = self.kickpro(tempat,pelaku,korban)).start()
        		if korban in self.bots:
        			if self.ac == True:A1 = threading.Thread(target=self.autoclearwar, args=(tempat,)).start();self.ac = False
        	return

    def komenbot(self, op):
        if op.type == 0:
            return
        elif op.type == 15:
        	if op.param2 in self.anti:
        		if self.type == 0:
        			try:self.client.inviteIntoGroup(op.param1,[op.param2]);self.client.sendMessage(op.param1,"Anti mode on")
        			except:self.client.sendMessage(op.param1,"Contact",{'mid': op.param2},13);self.client.sendMessage(op.param1,"please invite bot 👆")
        elif op.type == 5:
        	if self.add == True:
        		if op.param2 not in self.bots:
        		    self.client.sendMessage(op.param1,"Thanks you add me")
        elif op.type == 26:
        	try:threading.Thread(target = self.notified_receive_message(op,)).start()
        	except Exception as e:
        		if 'code=35' in str(e):
        			self.client.limit = True
        		print(str(e))
        	return
    
    
    
    
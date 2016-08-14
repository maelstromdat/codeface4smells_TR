import string
prev = ""
target = open("mail-clean.mbox", 'w')
for line in open("mail.mbox") :
    prev = filter(lambda x: x in string.printable, prev)
    target.write(prev)
    prev = line
target.write(prev)
target.close()
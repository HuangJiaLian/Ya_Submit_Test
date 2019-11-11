import os
f = open('f2.txt')
f1 = open('group.txt')
content = f.read()
content1 = f1.read()
f.close()
f1.close()
l = content.split('\n')
#print(l)
g = content1.split('\n')
for case in l:
        cmd = 'mkdir ' + case
        os.system(cmd)
        for case_1 in g:
                cmd4_1 = 'mkdir ./' + case + '/' + case_1
                os.system(cmd4_1)
                cmd4_2 = 'touch ./' + case + '/' + 'v.txt'
                os.system(cmd4_2)
        f2 = open(case + '/v.txt','w')
        f2.write(case[2:] + '\n')
        f2.close()
        print(case[2:] + '\n')


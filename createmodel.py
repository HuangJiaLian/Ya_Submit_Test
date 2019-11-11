'''
@Description: 
@Author: Jack Huang
@Github: https://github.com/HuangJiaLian
@Date: 2019-11-01 13:37:24
@LastEditors: Jack Huang
@LastEditTime: 2019-11-01 14:27:03
'''

import os

f1 = open('f2.txt')
f2 = open('group.txt')
content1 = f1.read()
content2 = f2.read()
f1.close()
f2.close()

l = content1.split('\n')
g = content2.split('\n')

for case1 in l:
    cmd1 = 'mkdir ' + case1
    os.system(cmd1)
    for case2 in g:
        # Distination dir

        # Not good example:
        # ddir = './' + case1 + '/' + case2 +'/'
        
        # Good example:
        ddir = os.path.join(case1,case2)
        
        # Create new dir
        cmd2 = 'mkdir ' + ddir
        os.system(cmd2)

        # Create and write to file
        f3 = open(os.path.join(ddir,'v.txt'),'w')
        f3.write(case1[2:] + '\n')
        f3.close()

        cmd4 = 'cp rannum.f90 ' + ddir
        os.system(cmd4)

        cmd5 = 'cp model* ' + ddir
        os.system(cmd5)

        # Compile fortan source code, and spicify the 
        # path of a.out by using -o option.  
        cmd6 = 'f95 ' + os.path.join(ddir, 'model*') + ' -o' + os.path.join(ddir,'a.out')
        os.system(cmd6)

        # Copy submit script
        cmd7 = 'cp ll.sh ' + ddir
        os.system(cmd7)
        
        # Submit your job
        cmd8 = 'qsub ' + os.path.join(ddir, 'll.sh')
        print(cmd8)
        
        # Uncomment the following line when you really want to submit your job.
        # os.system(cmd8)
        
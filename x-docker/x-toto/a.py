import time

fout=open("/codea/docker-out/outx.txt",'w')

for i in range(10):
    print("i = {0:03d}".format(i))
    fout.write("i = {0:03d}\n".format(i))
    time.sleep(1)

fout.close()
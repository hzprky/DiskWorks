# DiskWorks
DiskWorks is a program for file functions on the PC-1600. It is written in Z80 assembler

To assemble the software, you need the assembler for the Sharp PC-1600 from Klaus Ditze. As it is copyrighted, i cannot add the file here.
I use the PC-1600 with both memory slots (S1: and S2:) installed. S1: has a 32KB card and S2: has a 128KB card installed. The card in S1: is initialised with the command INIT "S1:","P",16 to reserve some space for the machine software. That leaves about 28KB memory for Basic programs.

The Basic programm DWA.BAS in the repository is to invoke the assembler and pass the module names to the assember:
10 IF PEEK &D700=69AND PEEK &D701=76AND PEEK &D702=83THEN 40
20 PRINT "kein Assembler geladen!"
30 END 
40 CALL &D500;#0,&80C5,&BFFF,#1,&80C5,&BFFF
50 REM BLOAD "S2:ASM
60 CALL &D500;">S2:DW.T,S2:DF.A,S2:IN.A,S2:KL.A,S2:NA.A,S2:RD.A,S2:DW.A,S2:DWX"
70 KBUFF$ ="BLOAD"+CHR$ 34+"S2:DWX"+CHR$ 13

Unfortunately most parts of my source codes are gone. My Sharp PC-1600 has been on a hibernation for 30 years and i did not do a backup of my RAM Disks and the code on it disaapered to /dev/nul
I have managed to get an old version of the software (v1.0) which i could assemble until the end. Unfortunately the assembling process hang in the end so that i do not know, if it was really successfull or not 100% completed. I will try to continue developing it further but first need to understand my own code. After 30 years my knowledge has disappeared to /dev/nul also.

However, i have added the version 1.0 and 2.0 as binary files which can directly been used by everyone interested. Also i have added a short manual for the DiskWorks version 2.0. You should use the v2.0 as it is definitly more powerfull and mature. v1.0 just to have it complete.

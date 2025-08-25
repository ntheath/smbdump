smbdump:  smbdump.obj
        link smbdump.obj;

smbdump.obj:  smbdump.asm smbios.inc equates.inc
        masm smbdump.asm;
clean:
        del *.obj
        del *.exe


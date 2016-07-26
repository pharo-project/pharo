This class defines magic numbers taken from the PKWARE ZIP Application 
Note [1] approved by ISO/IEC JTC 1/SC 34 N 1621 in July 2011 [2] to be 
the international normative reference for the zip file format.  A class 
imports these bindings as 'class variables' by including the following 
in its class definition: 
    poolDictionaries: 'ZipFileConstants' 
There is a single method on the class side to initialise the values. 

[1] http://www.pkware.com/documents/casestudies/APPNOTE.TXT
[2] http://www.itscj.ipsj.or.jp/sc34/open/1621.pdf

Following are pertinent extracts from the PKWARE ZIP Application Note.... 
All values are stored in little-endian byte order unless otherwise 
specified. 
A. local file header signature     4 bytes  (0x04034b50) 
C. Data descriptor: 
           crc-32                          4 bytes 
           compressed size                 4 bytes 
           uncompressed size               4 bytes 
F. central file header signature   4 bytes  (0x02014b50) 
I. end of central dir signature    4 bytes  (0x06054b50) 
J.  Explanation of fields: 
     * version made by (2 bytes) 
            0 - MS-DOS and OS/2 (FAT / VFAT / FAT32 file systems) 
            3 - UNIX 
            1,2,4-20 not implemented 
     * compression method: (2 bytes) 
            0 - The file is stored (no compression) 
            6 - The file is Imploded 
            8 - The file is Deflated 
            1-7,9-19,97,98 not implemented 
      * For Methods 8 and 9 - Deflating 
            Bit 2  Bit 1 
              0      0    Normal (-en) compression option was used. 
             0      1    Maximum (-exx/-ex) compression option was used. 
              1      0    Fast (-ef) compression option was used. 
             1      1    Super Fast (-es) compression option was 
used.                               
        * internal file attributes: (2 bytes) 
            Bits 1 and 2 are reserved for use by PKWARE. The lowest bit 
of this field indicates, if set, that the file is apparently an ASCII or 
text file.  If not set, that the file apparently contains binary data. 

Note the following do not appear in the PKWARE ZIP Application Note.   
Should they be defined elsewhere? 
"Unix permission bits" 
DefaultDirectoryPermissions    := 8r040755. 
DefaultFilePermissions        := 8r0100666. 
DirectoryAttrib         := 8r040000. 
FileAttrib             := 8r0100000. 
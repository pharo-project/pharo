"
This class defines magic numbers taken from the RFC1952 GZIP file format 
specification version 4.3 (1996) [1].  A class imports these constants 
as 'class variables' by including the following in its class definition: 
   poolDictionaries: 'GZipConstants' 
A method on the class side initialises the values. 

[1] http://www.ietf.org/rfc/rfc1952.txt  (Section 2.3.1 Member header 
and trailer) 
-------------8<----snip--------------- 

As an aside the following in [GzipConstants class >> initialize] does 
not match the specification for bit 5 as ""reserved"". 
    GZipEncryptFlag := 16r20.    ""Archive is encrypted"" 

I did find it defined here [2] & [3] however the FAQ [4] specifically 
says encryption is NOT part of the standard. 

This constant is only used in [GzipReadStream >> on:from:to] as... 
    (flags anyMask: GZipEncryptFlag) 
        ifTrue:[^self error:'Cannot decompress encrypted stream']. 

So perhaps its okay to leave but maybe some slight benefit from amending 
the text as follows.. 
    GZipEncryptFlag := 16r20.    ""Archive is encrypted.  Not supported. 
Not part of the standard."" 
    ifTrue:[^self error:'Cannot decompress encrypted stream. Encryption 
is not part of RFC1952']. 

It is a better presentation to a user if you can indicate that it is 
someone else's fault that their decompress failed, and not Pharo. 

[2] http://www.onicos.com/staff/iz/formats/gzip.html
[3] http://research.cs.wisc.edu/wpis/examples/pcca/gzip/gzip.h
[3] http://www.gzip.org/#faq15
"
Class {
	#name : #GZipConstants,
	#superclass : #SharedPool,
	#classVars : [
		'GZipAsciiFlag',
		'GZipCommentFlag',
		'GZipContinueFlag',
		'GZipDeflated',
		'GZipEncryptFlag',
		'GZipExtraField',
		'GZipMagic',
		'GZipNameFlag',
		'GZipReservedFlags'
	],
	#category : #'Compression-Streams'
}

{ #category : #'pool initialization' }
GZipConstants class >> gzipMagic [
	^GZipMagic
]

{ #category : #'pool initialization' }
GZipConstants class >> initialize [
	"GZipConstants initialize"
	GZipMagic := 16r8B1F.		"GZIP magic number"
	GZipDeflated := 8.			"Compression method"

	GZipAsciiFlag := 16r01.		"Contents is ASCII"
	GZipContinueFlag := 16r02.	"Part of a multi-part archive"
	GZipExtraField := 16r04.		"Archive has extra fields"
	GZipNameFlag := 16r08.		"Archive has original file name"
	GZipCommentFlag := 16r10.	"Archive has comment"
	GZipEncryptFlag := 16r20.	"Archive is encrypted"
	GZipReservedFlags := 16rC0.	"Reserved" 
]

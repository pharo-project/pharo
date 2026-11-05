"
I am signaled when an encrypted ZIP member is extracted without a password, or with a wrong one. See ZipCrypto and ZipArchiveMember>>#password:.
"
Class {
	#name : 'ZipPasswordError',
	#superclass : 'ZipArchiveError',
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

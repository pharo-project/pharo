bootstrap
patchUnhandledErrorManagement

	[	'script.st' asFileReference writeStreamDo: [ :str |
		str nextPutAll: '''From Pharo6.0 of 13 May 2016 [Latest update: #60267] on 25 October 2016 at 10:12:07.2002 am''!
	
!UnhandledError methodsFor: ''priv handling'' stamp: ''ChristopheDemarey 8/14/2015 16:06''!
defaultAction

| rep |
	rep := FileStream forceNewFileNamed: ''PharoDebug.log''.
	rep nextPutAll: ''Error:''; cr.
	rep nextPutAll: exception asString; cr.
	rep nextPutAll: thisContext stack size asString.
	thisContext stack copy withIndexDo: [:stck :i |
		[rep nextPutAll: i asString; space; nextPutAll: stck asString; cr] ifError: []].
	rep close. 
	Smalltalk exitFailure
! !'].
	
		self bootstrapImageEvalFile: 'script.st' save: true quit: true.
	] ensure: [ 'script.st' asFileReference ensureDelete ]
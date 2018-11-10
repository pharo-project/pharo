"
I create unborn projects from a commitish if it is HEAD and it does not have a '.project' file.
"
Class {
	#name : #IceUnbornProjectReader,
	#superclass : #IceProjectReader,
	#category : #'Iceberg-Project'
}

{ #category : #testing }
IceUnbornProjectReader >> canReadProjectFromCommitish: aCommitish [
	
	^ aCommitish isWorkingCopy
		and: [ (aCommitish fileSystem / '.project') exists not ]
]

{ #category : #accessing }
IceUnbornProjectReader >> priority [

	"The unborn project reader should be the default one in case no other matches"
	^ 100000
]

{ #category : #reading }
IceUnbornProjectReader >> readProjectFromCommitish: anIceCommitish [

	^ IceUnbornProject onCommitish: anIceCommitish
]

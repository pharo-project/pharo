"
I am a reader for invalid project commits.
I return an invalid project for those commits that are not the current checked out commit and do not have a .project file.
This is because an invalid project cannot be fixed since it is already in the history.
"
Class {
	#name : #IceInvalidProjectReader,
	#superclass : #IceProjectReader,
	#category : #'Iceberg-Project'
}

{ #category : #testing }
IceInvalidProjectReader >> canReadProjectFromCommitish: aCommitish [
	
	^ aCommitish isWorkingCopy not
		and: [ (aCommitish fileSystem / '.project') exists not ]
]

{ #category : #accessing }
IceInvalidProjectReader >> priority [

	"The unborn project reader should be the default one in case no other matches"
	^ 100000
]

{ #category : #reading }
IceInvalidProjectReader >> readProjectFromCommitish: aCommitish [

	^ IceInvalidProject onCommitish: aCommitish
]

"
The interim record of ancestry for a working copy, gets merged version added to the ancestry, and is used to create the VersionInfo when the working copy becomes a version. 
"
Class {
	#name : 'MCWorkingAncestry',
	#superclass : 'MCAncestry',
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'adding' }
MCWorkingAncestry >> addAncestor: aNode [
	ancestors := (self ancestors reject: [ :each | aNode hasAncestor: each ]) copyWith: aNode lazy
]

{ #category : 'adding' }
MCWorkingAncestry >> addStepChild: aVersionInfo [
	stepChildren := stepChildren copyWith: aVersionInfo lazy
]

{ #category : 'accessing' }
MCWorkingAncestry >> ancestors: anObject [
	ancestors := anObject collect: [ :a | a lazy ]
]

{ #category : 'accessing' }
MCWorkingAncestry >> infoWithName: nameString message: messageString [
	^ MCVersionInfo
		name: nameString
		id: UUID new
		message: messageString
		date: Date today
		time: Time now
		ancestors: ancestors asArray
		stepChildren: self stepChildren asArray
]

{ #category : 'initialization' }
MCWorkingAncestry >> lazy [
	^ self
]

{ #category : 'accessing' }
MCWorkingAncestry >> name [
	^ '<working copy>'
]

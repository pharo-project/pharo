"
Common superclass for commands on configuration infos in Versionner
"
Class {
	#name : #MBConfigurationInfoCommand,
	#superclass : #MBCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #ui }
MBConfigurationInfoCommand >> showExecuteWhile: aBlock [

	^Cursor execute showWhile:  aBlock
]

"
SUnit tests for MBGroupInfo in Versionner
"
Class {
	#name : #MBGroupInfoTest,
	#superclass : #MBSpecInfoTest,
	#category : 'Versionner-Tests-Core-Model'
}

{ #category : #tests }
MBGroupInfoTest >> testPrintOn [
	| str | 
	self shouldnt: [str := MBGroupInfo new printString] raise: Error.
	self assert: (str = 'nil ---').
	

]

{ #category : #tests }
MBGroupInfoTest >> testgroupContentsIfLoaded [

	self assert: MBGroupInfo new groupContentsIfLoaded isEmpty
]

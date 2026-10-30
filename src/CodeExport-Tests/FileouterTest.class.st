Class {
	#name : 'FileouterTest',
	#superclass : 'TestCase',
	#category : 'CodeExport-Tests-Base',
	#package : 'CodeExport-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FileouterTest >> testFileoutClassOnStream [
	| fileouter stream contents |
	fileouter := Fileouter new.
	fileouter addFullClass: Point.
	stream := String new writeStream.
	fileouter fileoutOnStream: stream.
	contents := stream contents.

	self assert: contents isNotEmpty.
	self assert: (contents includesSubstring: 'Object subclass: #Point')
]

{ #category : 'tests' }
FileouterTest >> testFileoutMethodOnStream [
	| fileouter stream contents |
	fileouter := Fileouter new.
	fileouter addMethod: (Point >> #x).
	stream := String new writeStream.
	fileouter fileoutOnStream: stream.
	contents := stream contents.

	self assert: contents isNotEmpty.
	self assert: (contents includesSubstring: 'Point methodsFor:').
	self assert: (contents includesSubstring: 'x')
]

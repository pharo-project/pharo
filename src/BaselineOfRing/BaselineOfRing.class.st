Class {
	#name : 'BaselineOfRing',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRing',
	#package : 'BaselineOfRing'
}

{ #category : 'accessing' }
BaselineOfRing >> baseline: spec [
	<baseline>
	spec for: #common
		do: [ 
	
			spec 
				package: 'Ring-Core';
				package: 'Ring-Monticello';
				package: 'Ring-ChunkImporter';
				package: 'Ring-Tests-Core';
				package: 'Ring-Tests-Monticello';
				package: 'Ring-Tests-ChunkImporter';
				package: 'Ring-TraitsSupport';
				package: 'Ring-RuntimeSupport';
				package: 'Calypso-Ring' ]
]

{ #category : 'accessing' }
BaselineOfRing >> repository [
	^ self packageRepositoryURL
]

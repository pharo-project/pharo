Class {
	#name : #BaselineOfRing,
	#superclass : #BaselineOf,
	#category : #BaselineOfRing
}

{ #category : #accessing }
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
				package: 'Ring-TraitsV2Support';
				package: 'Ring-RuntimeSupport';
				package: 'Calypso-Ring' ]
]

{ #category : #accessing }
BaselineOfRing >> repository [
	^ self packageRepositoryURL
]

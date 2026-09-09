Class {
	#name : 'BaselineOfZincCharacterEncoding',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfZincCharacterEncoding',
	#package : 'BaselineOfZincCharacterEncoding'
}

{ #category : 'baselines' }
BaselineOfZincCharacterEncoding >> baseline: spec [

    <baseline>

    spec for: #common do: [
        spec 
            package: 'Zinc-Character-Encoding-Core';
            package: 'Zinc-Character-Encoding-Tests' with: [ spec requires: #('Zinc-Character-Encoding-Core') ].
        
        spec 
            group: 'Core' with: #('Zinc-Character-Encoding-Core');
            group: 'Tests' with: #('Zinc-Character-Encoding-Tests') ]
]

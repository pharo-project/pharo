Class {
	#name : 'BaselineOfClapMockNoTestGroup',
	#superclass : 'BaselineOf',
	#category : 'SUnit-UI-Tests-Mocks',
	#package : 'SUnit-UI-Tests',
	#tag : 'Mocks'
}

{ #category : 'baselines' }
BaselineOfClapMockNoTestGroup >> baseline: spec [
    <baseline>
    spec for: #common do: [
        spec package: 'FakePackage1'.
        spec package: 'FakePackage2'.
        spec package: 'FakePackage-Tests' with: [ spec requires: #('FakePackage1') ].
        
        spec group: 'Core' with: #('FakePackage2').
    ]
]

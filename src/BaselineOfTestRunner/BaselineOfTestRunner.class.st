Class {
	#name : 'BaselineOfTestRunner',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfTestRunner',
	#package : 'BaselineOfTestRunner'
}

{ #category : 'baselines' }
BaselineOfTestRunner >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec
				package: 'TestRunner-Core';
				package: 'TestRunner-FakeTests';
				package: 'TestRunner-Tests' with: [ spec requires: #('TestRunner-Core' 'TestRunner-FakeTests') ] ].
			
			spec
				group: 'Core' with: #( 'TestRunner-Core' );
				group: 'Tests' with: #( 'TestRunner-Tests' 'TestRunner-FakeTests' );
				group: 'default' with: #( 'Core' 'Tests' );
				group: 'All' with: #( 'Core' 'Tests' )
]

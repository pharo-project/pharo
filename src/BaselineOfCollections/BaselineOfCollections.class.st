Class {
	#name : 'BaselineOfCollections',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCollections',
	#package : 'BaselineOfCollections'
}

{ #category : 'baselines' }
BaselineOfCollections >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Collections-Abstract';
				package: 'Collections-DoubleLinkedList';
				package: 'Collections-Native';
				package: 'Collections-Sequenceable';
				package: 'Collections-Streams';
				package: 'Collections-Strings';
				package: 'Collections-Support';
				package: 'Collections-Unordered';
				package: 'Collections-Weak';
				
				package: 'Collections-Arithmetic';
				package: 'Collections-Atomic';
				package: 'Collections-Stack';
				
				package: 'Collections-Abstract-Tests' with: [ spec requires: #( 'Collections-Abstract' ) ];
				package: 'Collections-DoubleLinkedList-Tests' with: [ spec requires: #( 'Collections-DoubleLinkedList' ) ];
				package: 'Collections-Native-Tests' with: [ spec requires: #( 'Collections-Native' ) ];
				package: 'Collections-Sequenceable-Tests' with: [ spec requires: #( 'Collections-Sequenceable') ];
				package: 'Collections-Streams-Tests' with: [ spec requires: #( 'Collections-Streams' ) ];
				package: 'Collections-Strings-Tests' with: [ spec requires: #( 'Collections-Strings' ) ];
				package: 'Collections-Support-Tests' with: [ spec requires: #( 'Collections-Support' ) ];
				package: 'Collections-Unordered-Tests' with: [ spec requires: #( 'Collections-Unordered' 'Collections-Arithmetic' 'Collections-Abstract-Tests') ];
				package: 'Collections-Weak-Tests' with: [ spec requires: #( 'Collections-Weak' 'Collections-Unordered-Tests') ];
				
				package: 'Collections-Arithmetic-Tests' with: [ spec requires: #( 'Collections-Arithmetic' ) ];
				package: 'Collections-Atomic-Tests' with: [ spec requires: #( 'Collections-Atomic' ) ];
				package: 'Collections-Stack-Tests'with: [ spec requires: #( 'Collections-Stack' ) ].
				

			spec 
            group: 'Kernel' with: #(
                'Collections-Abstract' 'Collections-DoubleLinkedList' 'Collections-Native' 
                'Collections-Sequenceable' 'Collections-Streams' 'Collections-Strings' 
                'Collections-Support' 'Collections-Unordered' 'Collections-Weak'
            );
            group: 'Additional' with: #(
                'Collections-Arithmetic' 'Collections-Atomic' 'Collections-Stack'
            );
            group: 'Core' with: #('Kernel' 'Additional');
            
            group: 'KernelTests' with: #(
                'Collections-Abstract-Tests' 'Collections-DoubleLinkedList-Tests' 
                'Collections-Native-Tests' 'Collections-Sequenceable-Tests' 
                'Collections-Streams-Tests' 'Collections-Strings-Tests' 
                'Collections-Support-Tests' 'Collections-Unordered-Tests' 
                'Collections-Weak-Tests'
            );
            group: 'AdditionalTests' with: #(
                'Collections-Arithmetic-Tests' 'Collections-Atomic-Tests' 'Collections-Stack-Tests'
            );
            group: 'Tests' with: #('KernelTests' 'AdditionalTests').]
]

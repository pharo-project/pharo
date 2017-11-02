Extension { #name : #VirtualMachine }

{ #category : #'*GT-InspectorExtensions-Core' }
VirtualMachine >> gtInspectorDetailsIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite table
		title: 'Details';
		display: [ 
			{ 
				'VM directory' -> self vmDirectory asFileReference.
				'VM build date' -> self buildDate.
				'VM version' -> self version.
				'Image directory' -> self imagePath asFileReference parent.
				'Image version' -> self imageVersionNumber.

			} ];
		column: 'Property' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]

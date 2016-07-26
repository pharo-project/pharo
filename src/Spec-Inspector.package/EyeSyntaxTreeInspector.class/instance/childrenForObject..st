api
childrenForObject: aNode
	^ aNode children collect: [ :each | 
			DynamicEyeElement
				host: aNode
				label: each class name
				description: [ :node | node formattedCode ]
				value: [each] ]
utilities
assert: aGofer repositories: anArray
	self assert: aGofer repositories size = anArray size.
	aGofer repositories with: anArray
		do: [ :first :second | 
			self assert: first description equals: second ]
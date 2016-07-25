private
do: aBlock with: aFirstElement

 	self do: aBlock with: aFirstElement when: [ :t | true ]. 
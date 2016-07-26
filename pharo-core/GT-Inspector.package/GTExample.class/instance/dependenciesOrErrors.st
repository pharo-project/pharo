private
dependenciesOrErrors
	^ self arguments collect: [ :each | 
		[ each example ] 
			on: Error 
			do: [ :exception | exception ] ]
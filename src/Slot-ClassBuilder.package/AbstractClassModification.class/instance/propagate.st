private
propagate
	target subclassesDo: [ :subclass |
		propagations add:
			(ClassModificationPropagation
				propagate: self
				to: subclass) ].
	"recursively propagate this change down"
	propagations do: [ :propagation | propagation propagate ]
	
		
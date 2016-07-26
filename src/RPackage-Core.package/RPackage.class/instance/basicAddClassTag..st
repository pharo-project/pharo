private
basicAddClassTag: tagName
	| packageTag |
			
	packageTag := RPackageTag package: self name: tagName.
	classTags add: packageTag.
	packageTag ensureSystemCategory.
	
	^ packageTag 
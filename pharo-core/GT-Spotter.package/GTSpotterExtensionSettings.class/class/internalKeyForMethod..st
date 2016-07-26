private
internalKeyForMethod: each
	^ (each methodClass name , '_' , each selector allButLast) asSymbol
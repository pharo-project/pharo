installing
isExternalStructureFieldDefinition
	"Really belongs in the FFI package, for now, include in base even if FFI is not loaded due to the dire consequences if its not here and an FFI package is loaded (incorrect field compilation can cause VM crashes).
	
	If in the future other external packages are found to need custom post-load blocks as well, a pluggable way of doing so should be investigated, but for now it's not worth the effort and coordination required with external package maintainers"
	^ selector = #fields
		and: [classIsMeta
			and: [
				(Smalltalk globals at: #ExternalStructure ifPresent: [:externalStructure |
					self actualClass theNonMetaClass inheritsFrom: externalStructure]) == true]]

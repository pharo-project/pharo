*Reflectivity
optionsFor: aLink
	| options |
	options := aLink options copy.
	options addAll: self metaLinkOptionsFromClassAndMethod.
	^options
	
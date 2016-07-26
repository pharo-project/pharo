zen
pharoZenValuesContents
	"Return a string with CRs listing the Pharo Zen values in random order"
	
	"self pharoZenValuesContents"
	
	^ String streamContents: [ :stream | 
			stream << (self class>>#pharoZenValues) comment withoutQuoting; cr; cr.
			self pharoZenValues shuffled 
				do: [ :each | stream << each ] 
				separatedBy: [ stream cr ] ]
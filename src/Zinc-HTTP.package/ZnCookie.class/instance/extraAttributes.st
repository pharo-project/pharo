private
extraAttributes
	"Answer all attributes except name and value"
	^ attributes associationsSelect: [ :ea | (ea key = 'name' or: [ ea key = 'value' ]) not ]
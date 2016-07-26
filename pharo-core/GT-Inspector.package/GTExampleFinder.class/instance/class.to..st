private
class: aClass to: examples
	aClass selectorsAndMethodsDo: [ :selector :method | 
		self class: aClass selector: selector method: method to: examples ] 
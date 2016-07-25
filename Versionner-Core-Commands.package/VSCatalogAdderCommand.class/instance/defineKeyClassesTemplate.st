metadata methods defintion
defineKeyClassesTemplate

	self configurationMetaclass 
		compile: 'catalogKeyClassesAndExample
	"Returns a paragraph or more describing the key classes of your project. You can use Pillar/Pier syntax to layout out the text i.e., ==Code== and - for bullet."
	^ ''MyProject is composed of one cool key class named ==MyProject==. There is also ==MyKeyHelper==
- ==MyProject== is the responsible for many cool aspects.
- ==MyKeyHelper== is helping ==MyProject=='''
		classified: 'catalog'
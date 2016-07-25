accessing
numberOfDisplayBlock: aBlock

	"this method allow the user to externally control the progression of this step. In merlin for exemple, it can be usefull to have some 'conditions' to validate before going on. If those conditions are not validated, we do not progress"
	numberOfDisplayBlock := aBlock
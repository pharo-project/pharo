A MTDependency is an abstraction for project dependencies.

Instance Variables
	- name : The dependency name
	- repository : The repository URL used to find this dependency
	- version : The specific version describes by the dependency
	- platforms : If not nil, the dependency is only applicable to specified platforms
	- dependencies : A dependency could have dependencies
	- project : The project root node
	- parent: The owner of the dependency.
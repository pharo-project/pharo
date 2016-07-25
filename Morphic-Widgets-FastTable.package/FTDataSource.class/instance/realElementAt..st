accessing
realElementAt: anIndex
	"I am here because some dataSource wrap there element into an other Object but we still want to access to the initial element. In that case a DataSource can override me to get the real element."

	^ self elementAt: anIndex
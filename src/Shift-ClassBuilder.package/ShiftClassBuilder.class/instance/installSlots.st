building
installSlots
	newClass classLayout slots do: [ :each | each installingIn: newClass ].
	newClass classLayout slots do: [ :each | each layoutChanged: newClass ].
	
	newClass class classLayout slots do: [ :each | each installingIn: newClass class ].
	newClass class classLayout slots do: [ :each | each layoutChanged: newClass class ].
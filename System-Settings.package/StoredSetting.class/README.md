I represent a part of SettingNode that can be stored to a preference file.

Responsibility: I keep SettingNode's stored value and I know to which SettingNode I belong.

Collaborators: 
 - SystemSettings manages all my instances, stores, load, and gives an appropriate stored value for any SettingNode.
 - StoredSettingBuilder creates instances of me.
 - I represent stored value of #realValue of a SettingNode object.

Public API and Key Messages
- realValue returns SettingNode's stored value.
- isForSettingNode: returns true if my instance belongs to a given SettingNode.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodClass:		<Class>
	selector:			<Symbol>
	realValue:		<Object>

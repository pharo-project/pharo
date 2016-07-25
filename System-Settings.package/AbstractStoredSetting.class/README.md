I am an abstract class of all stored settings. My subclasses are responsible for keeping SettingNode identifier and its real value that is supposed to store or has been loaded. There are some objects that cannot by directly stored into a file, e.g. fonts, because they includes too much information for storing. For that reason SettingNode objects are converted into objects of my subclasses that keeps storing simple. 

Collaborators: StoredSettingsFactory, SystemSettingsPersistence

Public API and Key Messages

- realValue
- settingNodeIdentifier 
- settingNodeIdentifier:
- isForSettingNode:
- StoredSettingsFactory creates instances of me.

Internal Representation and Key Implementation Points.

    Instance Variables
	settingNodeIdentifier:		<Symbol>

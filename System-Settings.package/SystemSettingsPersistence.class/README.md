I am a singleton responsible for storing and loading settings on a disk. I can handle a collection of settings or one setting in particular. I know where settings are stored.

External Collaborators (those that use me): SettingNode, SettingBrowser.

Internal Collaborators (those I use): SettingsStonReader, SettingsStonWriter, StoredSettingsFactory, StoredSettingsMerger

Public API and Key Messages

- updateSettingNodes:
- storeSettingNodes:
- storedValueForSettingNode:
- accessing instance: SystemSettingsPersistence default

Internal Representation and Key Implementation Points.

    Instance Variables
	fileReference:		<FileReference>
	settingTree:		<SettingTree>

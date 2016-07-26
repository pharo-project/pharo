I represent persistence strategy for GlobalIdentifier. 

I can load or save existing preferences (a dictionary) using #load: or #save: message. I know whare the preference file is stored (#preferences). I also know what previous persistence stategy was used; for that reason you should use #ensure: instead of #load: message. In that case, I will load the old file and then save it using new persistence strategy.

Collaborators: GlobalIdentifier uses me for loading and saving values; in both cases GlobalIdentifier uses #ensure:

Public API and Key Messages

- ensure: it loads stored values, merge with existing in the image and saves the current state on the disk

Example:
	GlobalIdentifierPersistence ston
		previous: (GlobalIdentifierPersistence fuel
					preferences: FileLocator workingDirectory / 'example.fuel';
					yourself);
		preferences: FileLocator workingDirectory / 'example.ston';
		ensure: (Dictionary new at: #a put: 1; at: #b put: 2);
		yourself

Internal Representation and Key Implementation Points.

    Instance Variables
	preferences:				<FileReference>
	previousPersistence:		<GlobalIdentifierPersistence>

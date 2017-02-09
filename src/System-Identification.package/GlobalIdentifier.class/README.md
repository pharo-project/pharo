I keep global IDs that are used for tracking user activity, e.g. computer id. By default, you should access me by calling #uniqueInstance.

Responsibility:
The IDs that are important to store onto disk (and shared by all images) should be placed in persistedInformation instance variable. On other hand, if you do not want to store it onto disk, create a new instance variable. I can #loadPreferences and #savePreferences onto a disk.

I know computer ID and secret ID. Computer ID is a global UUID that is share among all the images. It is stored on a local disk. Secret ID is use for encrypting information, e.g., class names, method names. You can use #hashForText: method.

Collaborators: I do not collaborate with other classes. I only offer the basic IDs for other frameworks.

Public API and Key Messages

- computerUUID 
- ensureComputerUUID
- hashForText:
- loadPreferences 
- savePreferences.

Before using #computerUUID, you should call #ensureComputerUUID. It will update UUID from the disk or stores existing one if it is not stored yet. I behave like this as automatic storing data to disk on image start-up leads to errors.

Internal Representation and Key Implementation Points.

    Instance Variables
	persistedInformation:		<Dictionary>
	preferences:		<FileReference>

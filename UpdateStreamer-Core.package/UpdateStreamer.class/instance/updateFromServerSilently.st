public
updateFromServerSilently
	"Scan the update server for unassimilated updates. If upToNumber is not nil, it represents the highest-numbered update to load.  This makes it possible to update only up to a particular point.   If saveLocally is true, then save local copies of the update files on disc.  If shouldApplyChanges is true, then absorb the updates into the current image.

A file on the server called updates.list has the names of the last N update files.  We look backwards for the first one we do not have, and start there"
"* To add a new update:  Name it starting with a new two-digit code.  
* Do not use %, /, *, space, or more than one period in the name of an update file.
* The update name does not need to have any relation to the version name.
* Figure out which versions of the system the update makes sense for.
* Add the name of the file to each version's category below.
* Put this file and the update file on all of the servers.
*
* To make a new version of the system:  Pick a name for it (no restrictions)
* Put # and exactly that name on a new line at the end of this file.
* During the release process, fill in exactly that name in the dialog box.
* Put this file on the server."

"When two sets of updates need to use the same directory, one of them has a * in its 
serverUrls description.  When that is true, the first word of the description is put on
the front of 'updates.list', and that is the index file used."

	self elementaryReadServerUpdates.
	^ true
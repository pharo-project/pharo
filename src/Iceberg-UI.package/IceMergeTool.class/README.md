This is a very basic merge tool, just for keeping some state and coordinating interation between the 'real' merge tools.
 
Internal Representation
- source: <IceCommit> indicating the source of the changes to be merged (TODO: should be a commitish).
- targetBranch: <IceBranch> where we want to merge the commits. If not provided the tool will raise a dialog to ask for it.
- onMerge: <block or symbol> will be executed after a successfull merge.


A SettingTree is responsible for the building of system setting trees from a collection of pragma.
Built tree nodes are instances of SettingNode. Built trees are suitable for tree browsing with the help of a MorphTreeMorph (see SettingBrowser>>treeMorphIn: and SettingBrowser>>roots to know how a SettingBrowser is initializing its MorphTreeMorph with the help of a SettingTree). 

Below, two examples of tree building with a SettingCollector are shown.
---------------
(SettingTree acceptableKeywords: #(#'systemsettings')) settingTreeRoots. "get system setting trees"
---------------

Instance Variables
	collector		<PragmaCollector>
	nodeList		<A list of SettingNode>

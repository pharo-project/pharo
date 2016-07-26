running
clearPackageCache
	| dir |
	dir := MCCacheRepository uniqueInstance directory.
"	(dir filesMatching: 'MonticelloMocks*') do: [:ea | ea ensureDeleted ]."
	(dir filesMatching: 'MonticelloTest*') do: [:ea | ea ensureDelete].
	(dir filesMatching: 'rev*') do: [:ea | ea ensureDelete].
	(dir filesMatching: 'foo-*') do: [:ea | ea ensureDelete].
	(dir filesMatching: 'foo2-*') do: [:ea | ea ensureDelete].
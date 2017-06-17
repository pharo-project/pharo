enum description
enumDecl
	^ #(
		GIT_CHECKOUT_NONE  0 "< default is a dry run, no actual updates "

		" Allow safe updates that cannot overwrite uncommitted data "
		GIT_CHECKOUT_SAFE  "1 bitShift: 0" 1

		" Allow all updates to force working directory to look like index "
		GIT_CHECKOUT_FORCE  "1 bitShift: 1" 2


		" Allow checkout to recreate missing files "
		GIT_CHECKOUT_RECREATE_MISSING  "1 bitShift: 2" 4

		" Allow checkout to make safe updates even if conflicts are found "
		GIT_CHECKOUT_ALLOW_CONFLICTS  "1 bitShift: 4" 16

		" Remove untracked files not in index (that are not ignored) "
		GIT_CHECKOUT_REMOVE_UNTRACKED  "1 bitShift: 5" 32

		" Remove ignored files not in index "
		GIT_CHECKOUT_REMOVE_IGNORED  "1 bitShift: 6" 64

		" Only update existing files, don't create new ones "
		GIT_CHECKOUT_UPDATE_ONLY  "1 bitShift: 7" 128

		"
		 * Normally checkout updates index entries as it goes; this stops that.
		 * Implies `GIT_CHECKOUT_DONT_WRITE_INDEX`.
		 "
		GIT_CHECKOUT_DONT_UPDATE_INDEX  "1 bitShift: 8" 256

		" Don't refresh index/config/etc before doing checkout "
		GIT_CHECKOUT_NO_REFRESH  "1 bitShift: 9" 512

		" Allow checkout to skip unmerged files "
		GIT_CHECKOUT_SKIP_UNMERGED  "1 bitShift: 10" 1024
		" For unmerged files, checkout stage 2 from index "
		GIT_CHECKOUT_USE_OURS  "1 bitShift: 11" 2048
		" For unmerged files, checkout stage 3 from index "
		GIT_CHECKOUT_USE_THEIRS  "1 bitShift: 12" 4096

		" Treat pathspec as simple list of exact match file paths "
		GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH  "1 bitShift: 13" 8192

		" Ignore directories in use, they will be left empty "
		GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES  "1 bitShift: 18" 262144

		" Don't overwrite ignored files that exist in the checkout target "
		GIT_CHECKOUT_DONT_OVERWRITE_IGNORED  "1 bitShift: 19" 524288

		" Write normal merge files for conflicts "
		GIT_CHECKOUT_CONFLICT_STYLE_MERGE  "1 bitShift: 20" 1048576

		" Include common ancestor data in diff3 format files for conflicts "
		GIT_CHECKOUT_CONFLICT_STYLE_DIFF3  "1 bitShift: 21" 2097152

		" Don't overwrite existing files or folders "
		GIT_CHECKOUT_DONT_REMOVE_EXISTING  "1 bitShift: 22" 4194304

		" Normally checkout writes the index upon completion; this prevents that. "
		GIT_CHECKOUT_DONT_WRITE_INDEX  "1 bitShift: 23" 8388608

		"
		 * THE FOLLOWING OPTIONS ARE NOT YET IMPLEMENTED
		 "

		" Recursively checkout submodules with same options (NOT IMPLEMENTED) "
		GIT_CHECKOUT_UPDATE_SUBMODULES  "1 bitShift: 16" 65536
		" Recursively checkout submodules if HEAD moved in super repo (NOT IMPLEMENTED) "
		GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED  "1 bitShift: 17" 131072
)
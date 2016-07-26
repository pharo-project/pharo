versions
latestVersion: blessing

	^(self map values select: [:version | blessing = version blessing ]) detectMax: [:version | version ]
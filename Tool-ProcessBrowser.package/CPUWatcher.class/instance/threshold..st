accessing
threshold: thresh
	"What fraction of the time can a process be the active process before we stop it?"
	threshold := (thresh max: 0.02) min: 1.0
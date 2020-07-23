package rtp

import (
	"github.com/mudutv/randutil"
)

// Use global random generator to properly seed by crypto grade random.
var globalMathRandomGenerator = randutil.NewMathRandomGenerator() // nolint:gochecknoglobals

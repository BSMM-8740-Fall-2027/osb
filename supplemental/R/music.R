# created October 3, 2025

# see https://flujoo.github.io/gm/reference/index.html

# C1, C♯1/D♭1, D1, D♯1/E♭1, E1, F1, F♯1/G♭1, G1, G♯1/A♭1, A1, A♯1/B♭1, B1

# MIDI note numbers are numerical representations of musical notes used in digital music production,
# ranging from 0 to 127. For example, Middle C is assigned the MIDI note number 60,
# but its octave designation can vary between C3 and C4 depending on the convention used.


music <-
  gm::Music() +
  gm::Meter(4, 4) +
  gm::Line(c("C5", "D5", "E5", "F5"))

gm::show(music)

# (2) ----

score_chr <- c( "C","D","E","F", "A", "B" )
score_chr <- paste0(score_chr,1)
music <-
  gm::Music() +
  gm::Meter(4, 4) +
  gm::Line( 1:10+60 )

gm::show(music)

# (3) ----

# Generate random steps: +1 or -1
n_steps <- 20
steps <- sample(c(-1, 1), size = n_steps, replace = TRUE)

music <-
  gm::Music() +
  gm::Meter(4, 4) +
  gm::Line( 60 + cumsum(steps) )

gm::show(music)

# (3) ----

# Generate random steps: +1 or -1
n_steps <- 32
steps <- sample(c(-1, 1), size = n_steps, replace = TRUE)

music <-
  gm::Music() +
  gm::Meter(4, 4) +
  gm::Line( 60 + cumsum(steps) )

gm::show(music)


{ texliveSmall }:
texliveSmall.withPackages (ps: [
  ps.latex-bin
  ps.fontspec
  ps.latexmk
  ps.luatex85
  ps.preview
  ps.tikz-cd
  ps.doublestroke
  ps.dvisvgm
  ps.rsfs
])

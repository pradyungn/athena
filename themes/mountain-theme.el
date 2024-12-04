;; mountain.el --- Mountain Theme
(let ((colors '((bg         '("#0f0f0f" "#0f0f0f"     "black"  ))
                (fg         '("#f0f0f0" "#f0f0f0"     "brightwhite"  ))

                ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                ;; and `doom-blend' helper functions.
                (bg-alt     '("#262626" "#262626"     "black"        ))
                (fg-alt     '("#e7e7e7" "#e7e7e7"     "white"        ))

                ;; These should represent a spectrum from bg to fg, where base0 is a starker
                ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                ;; dark grey, base0 should be white and base8 should be black.
                (base0      '("#191919" "#191919"     "black"        ))
                (base1      '("#262626" "#262626"     "brightblack"  ))
                (base2      '("#393939" "#393939"     "brightblack"  ))
                (base3      '("#4c4c4c" "#4c4c4c"     "brightblack"  ))
                (base4      '("#767676" "#767676"     "brightblack"  ))
                (base5      '("#a0a0a0" "#a0a0a0"     "brightblack"  ))
                (base6      '("#bfbfbf" "#bfbfbf"     "brightblack"  ))
                (base7      '("#cacaca" "#cacaca"     "brightblack"  ))
                (base8      '("#e7e7e7" "#e7e7e7"     "white"        ))

                (grey       base3)
                (red        '("#ac8a8c" "#ac8a8c" "red"          ))
                (orange     '("#c6a679" "#c6a679" "brightred"    ))
                (green      '("#8aac8b" "#8aac8b" "green"        ))
                (teal       '("#9ec49f" "#9ec49f" "brightgreen"  ))
                (yellow     '("#aca98a" "#aca98a" "yellow"       ))
                (blue       '("#a5b4cb" "#a5b4cb" "brightblue"   ))
                (dark-blue  '("#8a98ac" "#8a98ac" "blue"         ))
                (magenta    '("#ac8aac" "#ac8aac" "brightmagenta"))
                (violet     '("#8f8aac" "#8f8aac" "magenta"      ))
                (cyan       '("#9ec3c4" "#9ec3c4" "brightcyan"   ))
                (dark-cyan  '("#8aacab" "#8aacab" "cyan"         ))
                (scream     '("#c49ea0" "#c49ea0" "brightred"    ))
                (hono       '("#ceb188" "#ceb188" "brightyellow" ))

                ;; face categories
                (highlight      violet)
                (vertical-bar   base0)
                (selection      base1)
                (builtin        blue)
                (comments       grey)
                (doc-comments   (doom-lighten grey 0.14))
                (constants      orange)
                (functions      violet)
                (keywords       magenta)
                (methods        blue)
                (operators      fg)
                (type           yellow)
                (strings        green)
                (variables      fg)
                (numbers        orange)
                (region         selection)
                (error          scream)
                (warning        hono)
                (success        teal)
                (vc-modified    fg-alt)
                (vc-added       green)
                (vc-deleted     red)

                ;; org hierarchy
                (level1)
                (level2 dark-blue)
                (level3 green)
                (level4 yellow)
                (level5 magenta)
                (level6 dark-cyan)
                (level7 scream)
                (level8 green)
                (level9 blue)

                ;; custom categories
                (modeline-bg     base1)
                (modeline-bg-alt base1)
                (modeline-fg     fg)
                (modeline-fg-alt base4)))))

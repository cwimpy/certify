# ---------------------------------------------------------------------------
# MAKING CERTIFICATES WITH THE `certify` PACKAGE
# ---------------------------------------------------------------------------
#
# WHAT THIS SCRIPT DOES
#   Part 1 makes one certificate.
#   Part 2 makes a whole stack of them from a spreadsheet of names.
#
# HOW TO USE IT
#   1. Open this file in RStudio.
#   2. Run the SETUP section once (the first time only).
#   3. In Part 1, change the text between the quotation marks to whatever
#      you want on the certificate.
#   4. Select the whole Part 1 block and press Cmd+Return (Mac) or
#      Ctrl+Enter (Windows) to run it.
#   5. The finished PDF lands in a "certificates" folder on your Desktop.
#
# THE ONE RULE
#   Keep the quotation marks and the commas. Change only the words inside
#   the quotes. If R gives you an error, a missing quote or comma is the
#   culprit about 90% of the time.
#
# ---------------------------------------------------------------------------


# ===========================================================================
# SETUP -- run these two lines once, ever, then ignore this section forever
# ===========================================================================

# install.packages("certify")     # the certificate maker

library(certify)


# Where finished certificates get saved. Change this if you'd rather they
# went somewhere else. The folder is created for you if it doesn't exist.
out_dir <- "~/Desktop/certificates"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# ===========================================================================
# PART 1 -- ONE CERTIFICATE
# ===========================================================================
#
# Edit the words inside the quotes below, then run the whole block.
# Every line is optional except `path` and `recipient`. To leave something
# off the certificate entirely, delete that whole line (including its comma).

make_certificate(

  # --- Where the file goes, and what it's called ---------------------------
  path = file.path(out_dir, "jane-doe.pdf"),

  # --- Who it's for --------------------------------------------------------
  recipient = "Jane A. Doe",

  # --- The big heading across the top --------------------------------------
  title = "Certificate of Achievement",

  # --- The name of the award (printed in CAPITALS automatically) -----------
  award_name = "Outstanding Student",

  # --- Your school or organization -----------------------------------------
  # This prints across the header. If you use a logo instead (see the LOGO
  # note at the bottom of this file), this line is ignored.
  organization = "University of Somewhere",

  # --- Optional italic line under the award name ---------------------------
  program = "Bachelor of Arts in Political Science",

  # --- The "why they earned it" sentence -----------------------------------
  # Keep this to roughly one or two lines. It wraps automatically, but very
  # long text will start to crowd the signature area.
  citation = paste(
    "in recognition of exceptional dedication, scholarship,",
    "and service to the academic community."
  ),

  # --- Optional year line --------------------------------------------------
  academic_year = "2025-2026",

  # --- Who signs it --------------------------------------------------------
  # One signer gets a signature line on the left and a date line on the
  # right. For TWO signers, see the second example at the bottom.
  signers = list(
    list(name = "Alex Chair, Ph.D.", title = "Department Chair")
  ),

  # --- The color scheme ----------------------------------------------------
  # Your three choices:
  #   cert_theme_classic()  navy and gold on parchment
  #   cert_theme_formal()   black and silver on cream
  #   cert_theme_warm()     burgundy and gold on warm cream
  theme = cert_theme_classic()
)

# That's it. Open the Desktop "certificates" folder to see the result, or
# run this line to open the PDF right now:
# browseURL(file.path(out_dir, "jane-doe.pdf"))


# ===========================================================================
# PART 2 -- MANY CERTIFICATES FROM A SPREADSHEET
# ===========================================================================
#
# For a whole class or an awards ceremony. One spreadsheet of names in, one
# PDF per person out. No extra packages needed.
#
# STEP A: make the spreadsheet.
#   Run the block below. The first time, it creates a starter file called
#   roster.csv on your Desktop with two example rows. After that it leaves
#   your file alone, so it is always safe to re-run.

roster_path <- "~/Desktop/roster.csv"

if (!file.exists(roster_path)) {
  write.csv(
    data.frame(
      recipient  = c("Jane A. Doe", "John B. Smith"),
      award_name = c("Outstanding Student", "Outstanding Service"),
      citation   = c("for exceptional scholarship and service.",
                     "for years of devoted service.")
    ),
    roster_path, row.names = FALSE
  )
}

# STEP B: open roster.csv in Excel or Numbers and fill in your real names.
#   Keep the heading `recipient`. The `award_name` and `citation` columns are
#   optional -- drop them if everyone is getting the same award and wording.
#   Add as many rows as you need. Extra columns of your own are ignored (and
#   the ones being ignored get listed, which is how you catch a typo in a
#   heading). Save it as CSV.

# STEP C: run this to produce the certificates.

make_certificates(
  read.csv(roster_path),
  dir = out_dir,

  # Everything below is the same on every certificate. Edit to match
  # your event.
  title         = "Certificate of Achievement",
  organization  = "University of Somewhere",
  academic_year = "2025-2026",
  signers       = list(
    list(name = "Alex Chair, Ph.D.", title = "Department Chair")
  ),
  theme = cert_theme_classic()
)

# Files are named after the recipient: "Jane A. Doe" becomes jane-a-doe.pdf.
# Two people with the same name get -2 added rather than one overwriting the
# other. To choose the names yourself, add a `filename` column.

# Check your work -- this lists every PDF that was just created.
list.files(out_dir, pattern = "\\.pdf$")


# ===========================================================================
# EXTRAS -- copy any of these into the blocks above
# ===========================================================================

# --- TWO SIGNERS -----------------------------------------------------------
# Both signatures sit side by side and the date moves underneath them.
# The maximum is two.
#
#   signers = list(
#     list(name = "President Name", title = "President"),
#     list(name = "Secretary Name", title = "Secretary")
#   )

# --- A LOGO ----------------------------------------------------------------
# It must be a PNG file (.png, not .jpg). It replaces the organization name
# in the header, so you don't need both.
#
#   logo = "~/Desktop/our-logo.png"
#
# Not sure of the path? Run  file.choose()  in the console, pick the file,
# and R prints the path for you to copy.
#
# A logo with a transparent background looks best. One saved on solid white
# shows up as a white rectangle against the cream paper.
#
# SIZE: the logo is fitted inside a box 2 inches tall by 4.5 inches wide and
# keeps its proportions, so it never comes out stretched. A square or stacked
# logo ends up 2 inches tall; a long horizontal wordmark ends up 4.5 inches
# wide and shorter. To change that box:
#
#   logo_height = 1.4                  # smaller
#   logo_width  = 6                    # let a wide logo run closer to the edge

# --- YOUR OWN COLORS -------------------------------------------------------
# Build a palette from hex color codes. Only `primary` (borders and title)
# and `accent` (the metallic trim) really change the look; the rest have
# sensible defaults.
#
#   our_colors <- cert_theme(
#     primary    = "#003366",   # borders and the big title
#     accent     = "#c0a062",   # inner trim and corner flourishes
#     background = "#fbf8f1"    # the paper color
#   )
#
# then use  theme = our_colors  instead of  theme = cert_theme_classic()

# --- TURN OFF THE LAUREL BRANCHES ------------------------------------------
# The decorative sprigs on either side of the name.
#
#   show_laurels = FALSE

# --- A DIFFERENT PAPER SIZE ------------------------------------------------
# The default is US letter, landscape, which prints as-is on ordinary paper
# (choose "Actual Size" in the print dialog, not "Fit to Page").
#
#   paper = "a4"        # or "legal", "tabloid", "a3", "a5"
#
# For a size that is not on that list, give inches instead:
#
#   width = 12, height = 9

# --- A SPECIFIC DATE -------------------------------------------------------
# Otherwise it prints the current month and year.
#
#   date_str = "May 2026"

# --- IF R COMPLAINS ABOUT "cairo" ------------------------------------------
# On some computers the fancy PDF engine isn't installed. Add this line and
# it will use the plain one instead. The only difference is that dashes and
# curly quotes get simplified.
#
#   device = "pdf"

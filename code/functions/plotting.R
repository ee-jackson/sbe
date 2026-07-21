# Project-wide plotting conventions ----------------------------------------

sbe_colours <- c(
	"1-species" = "#009E73",
	"4-species" = "#E69F00FF",
	"16-species" = "#56B4E9FF",
	"16-species-cut" = "#F0E442"
)

sbe_labels <- c(
	"1-species" = "Single species",
	"4-species" = "4 Species mix",
	"16-species" = "16 Species mix",
	"16-species-cut" = "16 Species mix\nliana cutting",
	"compl" = "Complementarity effect",
	"selec" = "Selection effect",
	"net" = "Net biodiversity effect"
)

sbe_relabel <- function(x) {
	x <- as.character(x)
	matched <- match(x, names(sbe_labels))
	has_label <- !is.na(matched)

	x[has_label] <- unname(sbe_labels[matched[has_label]])
	x
}

sbe_labeller <- ggplot2::as_labeller(sbe_relabel)

theme_sbe <- function(
	base_size = 10,
	base_family = "",
	ink = "#202020",
	paper = "#FFFFFF",
	accent = "#D55E00FF"
) {
	ggplot2::theme_bw(
		base_size = base_size,
		base_family = base_family,
		ink = ink,
		paper = paper,
		accent = accent
	) +
		ggplot2::theme(
			geom = ggplot2::element_geom(pointshape = 16),
			palette.colour.discrete = palette.colors(palette = "Okabe-Ito")[-1],
			palette.fill.discrete = palette.colors(palette = "Okabe-Ito")[-1],
			palette.colour.continuous = scales::pal_viridis(),
			palette.fill.continuous = scales::pal_viridis(),
			legend.position = "bottom"
		)
}

scale_colour_sbe <- function(
	...,
	values = sbe_colours,
	labels = sbe_relabel
) {
	ggplot2::scale_colour_manual(
		...,
		values = values,
		labels = labels
	)
}

scale_fill_sbe <- function(
	...,
	values = sbe_colours,
	labels = sbe_relabel
) {
	ggplot2::scale_fill_manual(
		...,
		values = values,
		labels = labels
	)
}

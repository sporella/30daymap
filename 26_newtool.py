# #30DayMapChallenge
# Día 26: Nueva Herramienta -> Python
# Fragile States Index
# Datos: https://fragilestatesindex.org/excel/
# Autora: Stephanie Orellana (@sporella)

import pandas as pd
import geopandas as gp
import matplotlib.pyplot as plt
from hdx.location.country import Country
from pyproj import CRS

df = pd.read_excel("data/fsi-2020.xlsx")
df['iso_a3'] = df.apply(lambda row: Country.get_iso3_country_code_fuzzy(row["Country"])[0], axis=1)
world = gp.read_file(gp.datasets.get_path('naturalearth_lowres'))
world_dat = world.merge(df, on='iso_a3', how = "left")

gdf = world_dat.to_crs(CRS("ESRI:54009"))

plt.rcParams.update({
    "text.color": "black",
    "axes.facecolor": "black",
    "axes.edgecolor": "black",
    "axes.labelcolor": "white",
    "xtick.color": "white",
    "ytick.color": "white",
    "grid.color": "lightgray",
    "figure.facecolor": "black",
    "figure.edgecolor": "black",
    "savefig.facecolor": "black",
    "savefig.edgecolor": "black"})

text = "Sustainable                  Stable                                Warning                            Alert"


gdf.plot(column='Total', legend=True, edgecolor = "grey", cmap= 'BuPu_r', linewidth=0.1,
  legend_kwds={'label': "Fragile States Index", 'orientation': "horizontal"})
ax = plt.gca()
ax.axes.xaxis.set_visible(False)
ax.axes.yaxis.set_visible(False)
plt.figtext(0.98, .03, "@sporella", ha='right', color = "white", fontsize=6)
plt.figtext(0.5, 0.25, text, ha='center', color = "white", fontsize=8)
# plt.show()
plt.savefig("plots/26_fragilestate.png", dpi = 120)

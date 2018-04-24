fx = open("out/results/xAxis")
fy4 = open("out/results/kind4")
fy8 = open("out/results/kind8")
fy16 = open("out/results/kind16")

xAxis = map(x -> parse(Float64,x), readlines(fx))
yAxis4 = map(x -> parse(Float64,x), readlines(fy4))
yAxis8 = map(x -> parse(Float64,x), readlines(fy8))
yAxis16 = map(x -> parse(Float64,x), readlines(fy16))

close(fx)
close(fy4)
close(fy8)
close(fy16)

using Plots
plotlyjs()
using Formatting
plot(xAxis, yAxis4, label = "kind 4", yformatter = :scientific, title = "Average error")
plot!(xAxis, yAxis8, label="kind 8")
plot!(xAxis, yAxis16, label="kind 16")
using Rsvg
savefig("out/plots/kind_4_8_16.pdf")

using Plots
plotlyjs()
plot(xAxis, yAxis8, label = "kind 8", yformatter = :scientific, title = "Average error")
plot!(xAxis, yAxis16, label="kind 16")
using Rsvg
savefig("out/plots/kind_8_16.pdf")

yAxis16 = map(x -> Float64(x)*Float64(10.0^(31)), yAxis16)
using Plots
plotlyjs()
plot(xAxis, yAxis16, label="kind 16", title="Average error", yformatter = x -> format(Float64(x), suffix = "×10⁻³¹", precision = 1))
using Rsvg
savefig("out/plots/kind_16.pdf")

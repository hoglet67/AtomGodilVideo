package uk.co.acornatom.dcmcalc;

public class DCMCalc {

	public DCMCalc() {
	}

	public static final void main(String[] args) {
		// if (args.length != 2) {
		// System.err.println("usage: java -jar dcmcalc.jar <Clock Freq in Hz> <Desired Freq in Hz>");
		// System.exit(1);
		// }
		// double clockFreq = Double.parseDouble(args[0]);
		// double desiredFreq = Double.parseDouble(args[1]);

		double clockFreq = 49152000;
		double desiredFreq = 1600.0 * 39375000.0 / 11.0 / 228;
		System.out.println("ClockFreq: " + clockFreq);
		System.out.println("DesiredFreq: " + desiredFreq);
		double bestError = Double.MAX_VALUE;

		for (int externalDivider = 1; externalDivider <= 2048; externalDivider++) {
			for (int dcm1M = 2; dcm1M < 32; dcm1M++) {
				for (int dcm1D = 1; dcm1D < 32; dcm1D++) {
					for (int dcm2M = 2; dcm2M < 32; dcm2M++) {
						for (int dcm2D = 1; dcm2D < 32; dcm2D++) {
							double freq = clockFreq * dcm1M * dcm2M / (double) dcm1D / (double) dcm2D / (double) externalDivider;
							double error = (freq - desiredFreq) / desiredFreq;
							if (error <= 0 & Math.abs(error) <= Math.abs(bestError)) {
								bestError = error;
								System.out.println("Freq: " + freq + "; Error: " + 1000000.0 * error + "ppm; (" + dcm1M + "/"
										+ dcm1D + ") * (" + dcm2M + "/" + dcm2D + ") / " + externalDivider);
							}
						}
					}
				}
			}
		}
	}

}

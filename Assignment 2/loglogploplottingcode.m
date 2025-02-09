% Example data (replace these with your own data)
data1 = readtable('C:\fortran\Computational_Physics\Assignment 2\loglogplot.dat');
x =table2array(data1(:, 2));  % x data points
y =table2array(data1(:, 1));  % y data points
% Create the log-log plot
figure;
loglog(x, y, 'o', 'MarkerSize', 8);  % Plot the data points
xlabel('X-axis');
ylabel('Y-axis');
title('Log-Log Plot');
grid on;
hold on;

% Perform linear fitting in log-log space (taking log10 of both x and y)
log_x = log10(x);  % log(x)
log_y = log10(y);  % log(y)

% Fit a line to the log-transformed data (linear regression)
p = polyfit(log_x, log_y, 1);  % Linear fit (log(y) = m*log(x) + c)

% Extract the slope (m) and intercept (c)
m = p(1);  % Slope
c = p(2);  % Intercept

% Generate the fitted values (line) using the fitted slope and intercept
y_fit = 10.^c * x.^m;  % y = 10^c * x^m (back to original scale)

% Plot the fitted line on the log-log plot
loglog(x, y_fit, '-r', 'LineWidth', 2);  % Plot the fitted line in red

% Display the equation of the fit in the form y = mx + c (in log-log space)
% Equation of the line in log-log space is log10(y) = m*log10(x) + c
% Showing this as y = mx + c is a linearized version in the log-log space.
equation = sprintf('log(error) = %.2f * log(n) + %.2f', m, c);

% Adjust the position of the equation text on the plot
text_position_x = 50000;   % X-coordinate of the text (set the desired X position)
text_position_y = 10^-5;  % Y-coordinate of the text (set the desired Y position)

% Add the equation as text on the plot
text(text_position_x , text_position_y, equation, 'FontSize', 12, 'Color', 'blue', 'BackgroundColor', 'white', 'FontWeight', 'bold');

% Make sure the plot axis limits are appropriate for viewing the text
axis([min(x) max(x) min(y) max(y)]);  % Automatically adjust axis limits
